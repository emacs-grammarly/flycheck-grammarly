;;; flycheck-grammarly.el --- Grammarly support for Flycheck.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-06 18:08:01

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Grammarly support for Flycheck.
;; Keyword: grammar check
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (flycheck "0.14"))
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


;;;###autoload
(defun flycheck-grammarly-setup ()
  "Setup Grammarly integration for Flycheck."
  )


(provide 'flycheck-grammarly)
;;; flycheck-grammarly.el ends here
