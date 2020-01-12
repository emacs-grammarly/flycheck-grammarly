;;; flycheck-grammarly.el --- Grammarly support for Flycheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-06 18:08:01

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Grammarly support for Flycheck.
;; Keyword: grammar check
;; Version: 0.1.2
;; Package-Requires: ((emacs "25.1") (flycheck "0.14") (grammarly "0.0.1") (cl-lib "0.6"))
;; URL: https://github.com/jcs090218/flycheck-grammarly

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Grammarly support for Flycheck.
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'dom)

(require 'flycheck)
(require 'grammarly)


(defgroup flycheck-grammarly nil
  "Grammarly support for Flycheck."
  :prefix "flycheck-grammarly-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/jcs090218/flycheck-grammarly"))


(defcustom flycheck-grammarly-check-time 0.8
  "How long do we call request after we done typing."
  :type 'float
  :group 'flycheck-grammarly)


(defvar-local flycheck-grammarly--done-checking nil
  "Check if Grammarly API done checking.")

(defvar-local flycheck-grammarly--point-data '()
  "List of error/warning JSON data.")

(defvar-local flycheck-grammarly--last-buffer-string nil
  "Record the last buffer string.")

(defvar-local flycheck-grammarly--request-timer nil
  "Timer that will tell to do the request.")


(defun flycheck-grammarly--column-at-pos (&optional pt)
  "Column at PT."
  (unless pt (setq pt (point)))
  (save-excursion
    (goto-char pt)
    (current-column)))


(defun flycheck-grammarly--on-open ()
  "On open Grammarly API."
  (when flycheck-mode
    (message "[INFO] Start connecting to Grammarly API...")))

(defun flycheck-grammarly--on-message (data)
  "Received DATA from Grammarly API."
  (when flycheck-mode
    (message "[INFO] Receiving data from grammarly, level (%s)" (length flycheck-grammarly--point-data))
    (when (string-match-p "\"point\":" data)
      (push data flycheck-grammarly--point-data))))

(defun flycheck-grammarly--on-close ()
  "On close Grammarly API."
  (when flycheck-mode
    (setq flycheck-grammarly--done-checking t)
    (flycheck-mode 1)))

(defun flycheck-grammarly--limit-changes-buffer (str)
  "Minify the STR to check if any text changed."
  (with-temp-buffer
    (insert str)
    (delete-whitespace-rectangle (point-min) (point-max))
    (call-interactively #'mark-whole-buffer)
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))
    (buffer-string)))

(defun flycheck-grammarly--kill-timer ()
  "Kill the timer."
  (when (timerp flycheck-grammarly--request-timer)
    (cancel-timer flycheck-grammarly--request-timer)
    (setq flycheck-grammarly--request-timer nil)))

(defun flycheck-grammarly--reset-request ()
  "Reset some variables so the next time the user done typing can reuse."
  (message "[INFO] Reset!")
  (setq flycheck-grammarly--last-buffer-string (buffer-string))
  (setq flycheck-grammarly--point-data '())
  (setq flycheck-grammarly--done-checking nil))

(defun flycheck-grammarly--after-change-functions (&rest _)
  "After change function to check if content change."
  (unless (string=
           (flycheck-grammarly--limit-changes-buffer flycheck-grammarly--last-buffer-string)
           (flycheck-grammarly--limit-changes-buffer (buffer-string)))
    (flycheck-grammarly--kill-timer)
    (setq flycheck-grammarly--request-timer
          (run-with-timer flycheck-grammarly-check-time nil
                          'flycheck-grammarly--reset-request))))

(defun flycheck-grammarly--encode-char (char-code)
  "Turn CHAR-CODE to character string."
  (cl-case char-code
    (4194208 (cons " " 2))
    (4194201 (cons "'" 3))
    (t nil)))

(defun flycheck-grammarly--html-to-text (html)
  "Turn HTML to text."
  (with-temp-buffer
    (insert html)
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (let ((replace-data (flycheck-grammarly--encode-char (char-before))))
        (when replace-data
          (backward-delete-char (cdr replace-data))
          (insert (car replace-data))))
      (forward-char 1))
    (dom-texts (libxml-parse-html-region (point-min) (point-max)))))

(defun flycheck-grammarly--grab-info (data attr)
  "Grab value through ATTR key with DATA."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-from-string data)))
    (gethash attr json)))

(defun flycheck-grammarly--valid-description (desc)
  "Convert to valid description DESC."
  (replace-regexp-in-string "\n" "" desc))

(defun flycheck-grammarly--check-all ()
  "Check grammar for buffer document."
  (let ((check-list '()))
    (dolist (data flycheck-grammarly--point-data)
      (let* ((type (if (string-match-p "error" data) 'error 'warning))
             (pt (flycheck-grammarly--grab-info data "highlightBegin"))
             (ln (line-number-at-pos (1+ pt)))
             (col (flycheck-grammarly--column-at-pos (1+ pt)))
             (desc (flycheck-grammarly--html-to-text
                    (flycheck-grammarly--grab-info data "explanation"))))
        (setq desc (flycheck-grammarly--valid-description desc))
        (push (list ln col type desc) check-list)))
    check-list))

(defun flycheck-grammarly--start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (add-hook 'after-change-functions #'flycheck-grammarly--after-change-functions nil t)
  (unless flycheck-grammarly--done-checking
    (flycheck-grammarly--reset-request)
    (grammarly-check-text (buffer-string)))
  (funcall
   callback
   'finished
   (flycheck-increment-error-columns
    (mapcar
     (lambda (x)
       (apply #'flycheck-error-new-at `(,@x :checker ,checker)))
     (condition-case err
         (if flycheck-grammarly--done-checking
             (flycheck-grammarly--check-all)
           (flycheck-stop))
       (error
        (funcall callback 'errored (error-message-string err))
        (signal (car err) (cdr err))))))))

(flycheck-define-generic-checker 'grammarly-checker
  "Grammarly flycheck definition."
  :start #'flycheck-grammarly--start
  :modes '(text-mode latex-mode org-mode markdown-mode))

(add-to-list 'flycheck-checkers 'grammarly-checker)
(add-to-list 'grammarly-on-open-function-list 'flycheck-grammarly--on-open)
(add-to-list 'grammarly-on-message-function-list 'flycheck-grammarly--on-message)
(add-to-list 'grammarly-on-close-function-list 'flycheck-grammarly--on-close)


(provide 'flycheck-grammarly)
;;; flycheck-grammarly.el ends here
