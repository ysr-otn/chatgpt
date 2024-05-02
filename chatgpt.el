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
  ;; chatgpt-output に出力を文字列として格納する
  (setq chatgpt-output (concat chatgpt-output string)))

;;; ChatGPT へのコマンドラインインタフェースを用いてバッファに結果を表示
(defun chatgpt ()
  "Execute chatgpt using command line interface"
  (interactive)
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
		(buffer-name (concat "*ChatGPT(" chatgpt-log-name ")*")))
	(setq chatgpt-output "")
	(start-process "chatgpt" buffer-name chatgpt-command "-s" chatgpt-log-name user-message)
	(set-process-filter (get-process "chatgpt") 'chatgpt-filter)
	(set-process-sentinel
	 (get-process "chatgpt")
	 (lambda (p e)
	   (progn
		 (end-of-buffer)
		 (newline)
		 (insert "----------------------------------------------------------------\n")
		 (end-of-buffer)
		 (insert chatgpt-output))))	
	(switch-to-buffer buffer-name)))

(provide 'chatgpt)
