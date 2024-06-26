#! /usr/bin/env python

# MIT License
# 
# Copyright (c) 2024 Yoshihiro Ohtani
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import os, argparse, re
from os.path import dirname, basename
import copy

# for トークン解析
import tiktoken

# for 要約
from pysummarization.nlpbase.auto_abstractor import AutoAbstractor
from pysummarization.tokenizabledoc.mecab_tokenizer import MeCabTokenizer
from pysummarization.abstractabledoc.top_n_rank_abstractor import TopNRankAbstractor

# for 類似性フィルター
from pysummarization.nlp_base import NlpBase
from pysummarization.similarityfilter.tfidf_cosine import TfIdfCosine

# for OpenAI
from openai import OpenAI


# OpenAI のトークン数の上限 4096 を基準に，要約処理をする閾値
LIMIT_TOKEN_COUNT_TO_ABST = 4096 * 0.8

# 要約をしない直近のメッセージの割x合
UNABST_RATE = 0.1


# 各メッセージのトークン数をリストで返す
# - msg: OpenAI クライアントの messages 形式のリスト
def count_each_messages_token(msg):
  count_tbl = []
  for m in msg:
    count = len(encoding.encode(m['content']))
    count_tbl.append(count)
  return count_tbl

# AutoAbstractor() の要約の結果のテキストを結合する
# - abst: AutoAbstractor().summarize()
def concat_abst_text(abst):
  text = ''
  for a in abst['summarize_result']:
    text += a
  return text
  

# 各メッセージの content を要約を作成して置き換え
# - msg: OpenAI クライアントの messages 形式のリスト
def make_abst_each_messages(msg, aut_abst, abst_doc, sim_fil):
  for i in range(len(msg)):
    msg[i]['content'] = concat_abst_text( auto_abst.summarize(msg[i]['content'], abst_doc, sim_fil) )


# 各メッセージの content を結合する
# - msg: OpenAI クライアントの messages 形式のリスト
def concat_each_messages_content(msg):
  text = ''
  for m in msg:
    text += m['content']
  return text


# メッセージの履歴を読み出し
# - hist_file: 履歴ファイルのパス
def read_message_from_history(hist_file):
  # 履歴ファイルを読み込み
  history=''

  with open(history_path, 'r') as f:
    history = f.read()
  
  # 履歴から ChatGPT へのメッセージを作成
  msg = []
  sel = ''
  role = ''
  content = ''
  
  for h in history.split("\n"):
    if h == '@@@role:::':
      if role != '' and content != '':
        msg.append({'role': role, 'content': content.rstrip()})
      sel = 'role'
      role = ''
      content = ''
    elif h == '@@@content:::':
      sel = 'content'
      content = ''
    elif sel == 'role':
      role += h
    elif  sel == 'content':
      content += h + '\n'
    
  # メッセージに最後の履歴を追加
  if role != '' and content != '':
    msg.append({'role': role, 'content': content.rstrip()})

  return msg

# メッセージの履歴を書き込み
# - msg: OpenAI クライアントの messages 形式のリスト
# - hist_file: 履歴ファイルのパス
def write_message_to_history(msg, hist_file):
  with open(hist_file, 'w') as f:
    for m in msg:
      hist = '@@@role:::\n' + m['role'] + '\n' + '@@@content:::\n' + m['content'].rstrip() + '\n' + '\n'
      f.write(hist)



# 引数解析
parser = argparse.ArgumentParser(description='Input comment.')
parser.add_argument('input', nargs = '*')
parser.add_argument('-r', '--reset', action='store_true', help='Execute with reset chat log history.')
parser.add_argument('-s', '--select', help='Select chat log history file.')
parser.add_argument('-l', '--list', action='store_true', help='List chat log history files.')
parser.add_argument('-n', '--no-write', action='store_true', help='Don\'t write chat log history file.')
args = parser.parse_args()

# 履歴ファイルを設定
home = os.getenv("HOME")
history_path='history.log'

if args.select != None:
  history_path = args.select

if home != None and os.path.isdir(home + '/.ChatGPT'):
  history_path = home + '/.ChatGPT/' + history_path

if not re.match(r'.*\.log$', history_path):
  history_path += '.log'

# 履歴ファイルのリストを表示する場合は表示して終了
if args.list == True:
  for f in os.listdir(dirname(history_path)):
    print(f)
  exit()


# スペースで区切られたコメントを一つの文字列に結合
comment = ''
for i in args.input:
  comment += i + ' '
comment = comment.strip(' ')


# 引数でコメントが無かったら，コメントの入力を要求
while comment == '':
  print('Input comment.')
  comment = input()


# コメントを表示(User のコメントであることを [U] で指定)
print('[U]: ' + comment)

# リセットフラグが立っていない&ファイルが存在する場合履歴を読み込む
if os.path.isfile(history_path) and not args.reset:
  msg = read_message_from_history(history_path)
else:
  msg = []
  

# 以降では msg は要約される可能性があるので，オリジナルのメッセージを保存
msg_orig = copy.deepcopy(msg)


# トークン数を計測
encoding = tiktoken.encoding_for_model("gpt-3.5-turbo")
token_count = sum(count_each_messages_token(msg))

# トークン数が要約をする閾値を越えていたら要約を実施
if token_count > LIMIT_TOKEN_COUNT_TO_ABST:
  # 自動要約のオブジェクトを生成
  auto_abst = AutoAbstractor()
  # トークナイザー（単語分割）にMeCabを指定
  auto_abst.tokenizable_doc = MeCabTokenizer()
  # 文書の区切り文字を指定
  auto_abst.delimiter_list = ["\n"]
  # キュメントの抽象化、フィルタリングを行うオブジェクトを生成
  abst_doc = TopNRankAbstractor()
  
  # NLPのオブジェクト
  nlp_base = NlpBase()
  nlp_base.tokenizable_doc = MeCabTokenizer()
  # 類似性フィルター生成
  similarity_filter = TfIdfCosine()
  # NLPのオブジェクトを設定
  similarity_filter.nlp_base = nlp_base
  # 類似性の閾値設定
  similarity_filter.similarity_limit = 0.25
  
  
  # まずは先頭から old_msg_count 個のメッセージについて要約を実施
  old_msg_count = int(len(msg) * (1 - UNABST_RATE))
  make_abst_each_messages(msg[0:old_msg_count], auto_abst, abst_doc, similarity_filter)
  
  # 要約後のトークン数を計測
  new_token_count = sum(count_each_messages_token(msg))
  # 旧いメッセージの要約の content を結合
  old_msg_abst_text = concat_each_messages_content(msg[0:old_msg_count])
  
  
  # 旧いメッセージを要約した結果，まだトークン数が閾値を越えていたら旧いメッセージの全要約を作成
  if new_token_count  > LIMIT_TOKEN_COUNT_TO_ABST:
    # 全メッセージの場合は比較的余裕があるはずなので類似性フィルターは適用しない
    # (全メッセージのトークン数によっては類似性フィルターが必要になるかも…)
    msg_abst = concat_abst_text( auto_abst.summarize(old_msg_abst_text, abst_doc) )
    
    # msg を旧いメッセージの全要約で初期化し，以降にオリジナルのメッセージから
    # 要約していないメッセージを結合
    msg = [{'role': 'assistant', 'content': msg_abst}]
    for i in range(old_msg_count, len(msg_orig)):
      msg.append(msg_orig[i])


# メッセージに，新規の質問を追加
msg.append({"role": "user", "content": comment})
msg_orig.append({"role": "user", "content": comment})


# ChatGPT に質問
client = OpenAI()

response = client.chat.completions.create(
  model="gpt-3.5-turbo",
  messages=msg
)


# 回答を取得 & 表示(Agent の回答であることを [A] で指定)
ans = response.choices[0].message.content
print('')
print('[A]: ' + ans)


# オリジナルのメッセージに回答を追加
msg_orig.append({"role": "assistant", "content": ans})




# 履歴を保存
if args.no_write == False:
  write_message_to_history(msg_orig, history_path)

  # 要約をしていたら参考のために要約の履歴も保存
  if token_count > LIMIT_TOKEN_COUNT_TO_ABST:
    # メッセージに回答を追加
    msg.append({"role": "assistant", "content": ans})
    write_message_to_history(msg, history_path.replace('.log', '_abst.log'))
