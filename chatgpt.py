#! /usr/bin/env python

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


# 履歴ファイルを読み込み
history=''
# リセットフラグが立っていない&ファイルが存在する場合履歴を読み込む
if os.path.isfile(history_path) and not args.reset:
  with open(history_path, 'r') as f:
    history = f.read()


# スペースで区切られたコメントを一つの文字列に結合
comment = ''
for i in args.input:
  comment += i + ' '
comment = comment.strip(' ')


# 引数でコメントが無かったら，コメントの入力を要求
while comment == '':
  print('Input comment.')
  comment = input()


# コメントを表示
print(comment)


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

# 以降では msg は要約される可能性があるので，オリジナルのメッセージを保存
msg_orig = copy.deepcopy(msg)


# トークン数を計測
encoding = tiktoken.encoding_for_model("gpt-3.5-turbo")
token_count = 0
message_all = ''

for m in msg:
  message_all += m['content']
  token_count += len(encoding.encode(m['content']))


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
  
  
  # まずは各メッセージについて要約を実施
  abst_token_count = 0
  for i in range(len(msg)):
    # 各メッセージをできるだけ簡潔にするために類似性フィルターを適用
    message_abst = auto_abst.summarize(msg[i]['content'], abst_doc, similarity_filter)
    msg_abst = ''
    for a in message_abst['summarize_result']:
      msg_abst += a
  
    abst_token_count += len(encoding.encode(msg_abst))
    
    msg[i]['content'] = msg_abst
  
  
  # 各メッセージを要約した結果，まだトークン数が閾値を越えていたら全メッセージの要約を作成
  if abst_token_count > LIMIT_TOKEN_COUNT_TO_ABST:
    # 全メッセージの場合は比較的余裕があるはずなので類似性フィルターは適用しない
    # (全メッセージのトークン数によっては類似性フィルターが必要になるかも…)
    all_abst = auto_abst.summarize(message_all, abst_doc)
    msg_abst = ''
    for a in all_abst['summarize_result']:
      msg_abst += a        
    
    msg = [{'role': 'assistant', 'content': msg_abst}]


# メッセージに，新規の質問を追加
msg.append({"role": "user", "content": comment})
msg_orig.append({"role": "user", "content": comment})


# ChatGPT に質問
client = OpenAI()

response = client.chat.completions.create(
  model="gpt-3.5-turbo",
  messages=msg
)


# 回答を取得 & 表示
ans = response.choices[0].message.content
print('')
print(ans)


# オリジナルのメッセージに回答を追加
msg_orig.append({"role": "assistant", "content": ans})


# 履歴を保存
if args.no_write == False:
  with open(history_path, 'w') as f:
    for m in msg_orig:
      hist = '@@@role:::\n' + m['role'] + '\n' + '@@@content:::\n' + m['content'].rstrip() + '\n' + '\n'
      f.write(hist)
