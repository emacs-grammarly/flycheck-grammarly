;; -*- mode: eask; lexical-binding: t -*-

(package "flycheck-grammarly"
         "0.2.3"
         "Grammarly support for Flycheck")

(website-url "https://github.com/emacs-grammarly/flycheck-grammarly")
(keywords "convenience" "grammar" "check")

(package-file "flycheck-grammarly.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "flycheck")
(depends-on "grammarly")
(depends-on "s")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
