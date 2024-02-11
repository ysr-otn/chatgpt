#! /usr/bin/env python

import os, argparse, re
from os.path import dirname, basename
from openai import OpenAI


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


while comment == '':
  print('Input comment.')
  comment = input()

# 質問を表示
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


# メッセージに，新規の質問を追加
msg.append({"role": "user", "content": comment})


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

# 回答をメッセージに追加
msg.append({"role": "assistant", "content": ans})


# 履歴を保存
if args.no_write == False:
  with open(history_path, 'w') as f:
    for m in msg:
      hist = '@@@role:::\n' + m['role'] + '\n' + '@@@content:::\n' + m['content'].rstrip() + '\n' + '\n'
      f.write(hist)
