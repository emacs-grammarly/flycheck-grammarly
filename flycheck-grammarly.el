;;; flycheck-grammarly.el --- Grammarly support for Flycheck.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-06 18:08:01

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Grammarly support for Flycheck.
;; Keyword: grammar check
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (flycheck "0.14") (grammarly "0.0.1"))
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

(require 'flycheck)
(require 'grammarly)


(defgroup flycheck-grammarly nil
  "Grammarly support for Flycheck."
  :prefix "flycheck-grammarly-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/jcs090218/flycheck-grammarly"))

(defvar-local flycheck-grammarly--done-checking nil
  "Check if Grammarly API done checking.")


(defun flycheck-grammarly--on-message (data)
  "Received DATA from Grammarly API."
  (message "[DATA] %s" data))

(defun flycheck-grammarly--on-close ()
  "On closse Grammarly API."
  (setq flycheck-grammarly--done-checking t)
  (flycheck-mode 1))

(defun flycheck-grammarly--after-change-functions (&rest _)
  "After change function to check if content changes."
  (setq flycheck-grammarly--done-checking nil))

(defun flycheck-grammarly--check-all ()
  "Check grammar for BUF document."
  (list (list 1 2 'error "Something went wrong here.")))

(defun flycheck-grammarly--start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (add-hook 'after-change-functions 'flycheck-grammarly--after-change-functions nil t)
  (unless flycheck-grammarly--done-checking
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
           (flycheck-mode -1))
       (error
        (funcall callback 'errored (error-message-string err))
        (signal (car err) (cdr err))))))))

(flycheck-define-generic-checker 'grammarly-checker
  "Grammarly flycheck definition."
  :start #'flycheck-grammarly--start
  :modes '(text-mode latex-mode org-mode markdown-mode))

(add-to-list 'flycheck-checkers 'grammarly-checker)
(add-to-list 'grammarly-on-message-function-list 'flycheck-grammarly--on-message)
(add-to-list 'grammarly-on-close-function-list 'flycheck-grammarly--on-close)


(provide 'flycheck-grammarly)
;;; flycheck-grammarly.el ends here
