;;; init.el --- emacs configuration file
;; (C) Copyright 2009, 2010 Ji Han (jihan917<at>yahoo<dot>com)
;; free to distribute under the GPL license


;; encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; package load path
;; {{{
(funcall
 '(lambda ()
    (let ((old-dir default-directory))
      (cd "~/.emacs.d/site-lisp")
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path)
      (cd old-dir))))
;; }}}

;; look-and-feel under windowing system
;; {{{
(if window-system
  (progn
    ;; no tool bar
    (tool-bar-mode nil)

    ;; scroll bar
    (scroll-bar-mode 'right)
    (setq scroll-conservatively 300)
    (setq scroll-preserve-screen-position 1)

    ;; enable clipboard
    (setq x-select-enable-clipboard t)

    ;; font
    (set-frame-font "Monaco")

    ;; color theme
    (require 'color-theme)
    (setq color-theme-load-all-themes nil)
    (eval-after-load "color-theme"
     '(progn
        (color-theme-initialize)
        (color-theme-ruby-blue)))

    ;; dynamic cursor
    (add-hook 'post-command-hook
     '(lambda ()
        (cond (buffer-read-only (setq cursor-type 'hbar))
              (overwrite-mode (setq cursor-type 'box))
              (t (setq cursor-type 'bar)))))))
;; }}}

;; do not echo shell command
;; {{{
(add-hook 'comint-mode-hook
 '(lambda ()
    (setq comint-process-echoes t)))
;; }}}

;; do not display startup mesage
(setq inhibit-startup-message t)

;; visible bell instead of ring bell on error
(setq visible-bell t)

;; highlight selection
(transient-mark-mode t)

;; answer yes-no question simply with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; don't append new lines when attempting to navigate beyond the last line
(setq next-line-add-newlines nil)

;; enforce that files shall end with newline
(setq require-final-newline t)

;; backup files
;; {{{
(setq make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/"))
;; }}}

;; recent files
;; {{{
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
;; }}}

;; vanilla Emacs time stamp ("Time-stamp: <>" in the first eight lines.)
(add-hook 'before-save-hook 'time-stamp)

;; however, I prefer my own time stamps:
;; {{{
(defun today ()
  "insert current date as string, formatted like Sunday, October 10, 2010."
  (interactive "p")
  (insert (format-time-string "%A, %B %e, %Y")))

(defun now ()
  "insert current time as string, formatted like 10:10 AM."
  (interactive "p")
  (insert (format-time-string "%_I:%M %p")))
;; }}}

;; header2
;; {{{
(require 'header2)
(add-hook 'write-file-hooks 'auto-update-file-header)
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook 'auto-make-header)
;; }}}

;; use text-mode as default
(setq-default major-mode 'text-mode)

;; turn on auto-fill
;; {{{
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 76)
;; }}}

;; tab expansion
;; {{{
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list nil)
;; }}}

;; display current date, time and cursor position
;; {{{
(setq line-number-mode t)
(setq column-number-mode t)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; }}}

;; display line numbers
(global-linum-mode 1)

;; syntax highlighting
(global-font-lock-mode t)

;; htmlize --- converting highlighted text to html
;; {{{
(require 'htmlize)
(setq htmlize-output-type "inline-css")
;; }}}

;; autopair
;; {{{
(require 'autopair)
(autopair-global-mode)
;; }}}

;; show matching parentheses
(show-paren-mode t)

;; jump between matching parentheses
;; {{{
(defun goto-matching-paren (arg)
  "jump to the matching parenthesis."
  (interactive "p")
  (cond
    ((looking-at "(")(progn (forward-sexp 1)(backward-char 1)))
    ((looking-at ")")(progn (forward-char 1)(backward-sexp 1)))
    (t nil)))

(global-set-key (kbd "C-]") 'goto-matching-paren)
;; }}}

;; org-mode
;; {{{
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; }}}

;; AucTex
;; {{{
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; }}}

;; C/C++ indentation
;; {{{
(add-hook 'c-mode-hook
 '(lambda()
    (c-set-style "k&r")))

(add-hook 'c++-mode-hook
 '(lambda()
    (c-set-style "stroustrup")))
;; }}}

;; hungry delete
(setq c-hungry-delete-key t)

;; compile single file if there's no makefile
;; {{{
(add-hook 'c++-mode-hook
 '(lambda ()
    (unless (or (file-exists-p "makefile") (file-exists-p "Makefile"))
      (set (make-local-variable 'compile-command)
        (let ((file (file-name-nondirectory buffer-file-name)))
          (concat "g++ -g -o " (file-name-sans-extension file) " " file))))))
;; }}}

;; CMake
;; {{{
(require 'cmake-mode)
(setq auto-mode-alist
  (append 
    '(("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    auto-mode-alist))
;; }}}

;; msf-abbrev
;; {{{
(require 'msf-abbrev)
(global-msf-abbrev-mode t)
;; }}}

;; CEDET
;; {{{
;;(load "~/.emacs.d/user-cedet-conf")
(require 'cedet)
(semantic-mode 1)
(require 'eassist)
(add-hook 'c-mode-common-hook
 '(lambda ()
    (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
    (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods)))
(autoload 'company-mode "company" nil t)
;; }}}

;; ECB
(require 'ecb-autoloads)

;; Haskell mode
;; {{{
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; }}}

;; Tuareg
;; {{{
(load "append-tuareg")
(load "custom-tuareg")
;; }}}

;; SLIME
;; {{{
;; (set-language-environment "utf-8")
(setq inferior-lisp-program
  (cond ((string= window-system "w32") "/opt/ccl/wx86cl -K utf-8")
        (t "/opt/ccl/lx86cl64 -K utf-8")))
(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))
;; }}}

;; move the line(s) spanned by the active region up/down
(load "~/.emacs.d/move-lines")

;; user-defined shortcut keys
(load "~/.emacs.d/user-keymaps")

;; defadvice customizations
(load "~/.emacs.d/user-advices")

;; keep byte-compiled dotemacs up to date
;; {{{
(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
    (when (equal buffer-file-name user-init-file)
      (add-hook 'after-save-hook
        '(lambda ()
          (let ((dotemacs (file-name-sans-extension user-init-file)))
            (byte-compile-file user-init-file)))))))
;; }}}

;; last updated: Thursday, November  4, 2010
;;; init.el ends here
