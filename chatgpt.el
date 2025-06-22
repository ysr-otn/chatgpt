;;; MIT License
;;; 
;;; Copyright (c) 2024 Yoshihiro Ohtani
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

;;; デフォルトのログ名称
(defvar chatgpt-log-name "history")

;;; ChatGPT へのコマンドラインインタフェース chatgpt.py のコマンド
(defvar chatgpt-command "chatgpt")

;;; chatgpt のプロセスのフィルタ
(defun chatgpt-filter (proc string)
  ;; Python の RuntimeWarning を除く出力を格納
  (if (not (string-match ": RuntimeWarning: " string))
	  ;; chatgpt-output に出力を文字列として格納する
	  (setq chatgpt-output (concat chatgpt-output string))))


;;; ChatGPT へのコマンドラインインタフェースを用いてバッファに結果を表示
(defun chatgpt (arg)
  "Execute chatgpt using command line interface"
  (interactive "p")
  ;; ~/.ChatGPT 以下のログファイルを選択(.log は省略．_abst.log は対象外)
  (setq chatgpt-log-name 
		(completing-read "Select Logfile: " 
						 (let ((log-name-comp nil)
							   (num 1)
							   (files (directory-files "~/.ChatGPT/")))
						   (mapcar (lambda (x)
									 (if (and (s-match "[A-z0-9_]+\\.log$" x)
											  (not (s-match "[A-z0-9_]+_abst\\.log$" x))
											  )
										 (progn
										   (add-to-list 'log-name-comp
														(list (string-replace ".log" "" x) num) t)
										   (setq num (1+ num)))))
								   files)
						   log-name-comp)
						 nil nil chatgpt-log-name
						 ))
  ;;; chatgpt.py を実行し *ChatGTP(log-name)* バッファに結果を表示
  (let ((user-message (read-string "Message: " nil))
		(buf-name (concat "*ChatGPT(" chatgpt-log-name ")*")))
	(setq chatgpt-output "")
	;;; 引数が設定されていれば結果をファイルに保存しないよう -n オプションを追加
	(if (eq arg 4)
		(start-process "chatgpt" buf-name chatgpt-command "-n" "-s" chatgpt-log-name user-message)
	  (start-process "chatgpt" buf-name chatgpt-command "-s" chatgpt-log-name user-message))
	(set-process-filter (get-process "chatgpt") 'chatgpt-filter)
	(set-process-sentinel
	 (get-process "chatgpt")
	 (lambda (p e)
	   (progn
		 ;; バッファを *ChatGTP(log-name)* に切り替え
		 (switch-to-buffer (concat "*ChatGPT(" chatgpt-log-name ")*"))
		 (end-of-buffer)
		 (newline)
		 (insert "----------------------------------------------------------------\n")
		 (end-of-buffer)
		 (insert chatgpt-output))))	
	(switch-to-buffer buf-name)))


;;; ChatGPT 用のバッファに新しいユーザメッセージの入力プロンプトを作成
(defun chatgpt-create-new-input-prompt ()
  (interactive)
  (if (string-match "\*ChatGPT\(.*\)\*" (buffer-name))
	  (progn
		(end-of-buffer)
		(newline)
		(insert "\n----------------------------------------------------------------\n[U]: ")
		(end-of-buffer))
	;;; *ChatGPT(XXX)* のバッファでなければエラー
	(message "Execute chatgpt-create-new-input command in the buffer of *ChatGPT(XXX)*")))

;;; chatgpt-create-new-input-prompt で作成したプロンプトに入力されたユーザメッセージを取得し，
;;; ChatGPT へのコマンドラインインタフェースを用いてバッファに結果を表示
(defun chatgpt-get-new-input-and-output-to-chatgpt-buffer (arg)
  (interactive "p")
  ;; *ChatGPT(XXX)* のバッファから実行
  (if (string-match "\*ChatGPT\(.*\)\*" (buffer-name))
	  (progn
		(goto-char (point-max))
		(let ((start (search-backward-regexp "^\\[U\\]:" nil t))
			  (end (point-max))
			  (buf-name (buffer-name)))
		  ;;[U]: の入力プロンプト発見
		  (if start
			  ;; 入力プロンプトからユーザのメッセージを取得
			  (let* ((user-message (replace-regexp-in-string "\[U\]: " "" (buffer-substring start (point-max)))))
				(setq chatgpt-log-name (replace-regexp-in-string "\)\\*" "" (replace-regexp-in-string "\\*ChatGPT\(" "" buf-name)))
				(end-of-buffer)
				(newline)
				(setq chatgpt-user-message user-message)
				(setq chatgpt-output "")
				;; 引数が設定されていれば結果をファイルに保存しないよう -n オプションを追加
				(if (eq arg 4)
					(start-process "chatgpt" buf-name chatgpt-command "-c" "-n" "-s" chatgpt-log-name user-message)
				  (start-process "chatgpt" buf-name chatgpt-command "-c" "-s" chatgpt-log-name user-message))
				;;; chatgpt.py を実行し *ChatGTP(log-name)* バッファに結果を表示
				(set-process-filter (get-process "chatgpt") 'chatgpt-filter)
				(set-process-sentinel
				 (get-process "chatgpt")
				 (lambda (p e)
				   (progn
					 ;; バッファを *ChatGTP(log-name)* に切り替え
					 (switch-to-buffer (concat "*ChatGPT(" chatgpt-log-name ")*"))
					 (end-of-buffer)
					 (insert chatgpt-output))))	
				(switch-to-buffer buf-name))
			;; [U]: の入力プロンプトが無ければエラー
			(message "Insert ChatGPT user message using chatgpt-create-new-input"))))
	;; *ChatGPT(XXX)* のバッファでなければエラー
	(message "Execute chatgpt-get-new-input-and-output-to-chatgpt-buffer command in the buffer of *ChatGPT(XXX)*")))


(provide 'chatgpt)
