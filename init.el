;;; init.el --- emacs configuration file
;; (C) Copyright 2009, 2010 Ji Han (jihan917<at>yahoo<dot>com)
;; Last Updated: Mon, 08 Nov 2010 21:37:56

;; custom-set-variables and custom-set-faces
;; {{{
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))
 '(before-save-hook (quote (time-stamp)))
 '(c-hungry-delete t)
 '(column-number-mode t)
 '(comint-process-echoes t)
 '(current-language-environment "UTF-8")
 '(delete-old-versions t)
 '(dired-copy-preserve-time t)
 '(dired-recursive-copy (quote always))
 '(dired-recursive-deletes (quote top))
 '(display-time-mode 1)
 '(fill-column 78)
 '(global-font-lock-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(hippie-expand-try-functions-list (quote (senator-try-expand-semantic try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(major-mode (quote text-mode))
 '(make-backup-files t)
 '(next-line-add-newlines nil)
 '(recentf-auto-cleanup (quote never))
 '(recentf-mode 1)
 '(require-final-newline t)
 '(show-paren-mode t)
 '(tab-always-indent (quote complete))
 '(tab-stop-list nil)
 '(tab-width 4)
 '(text-mode-hook (quote (turn-on-flyspell turn-on-auto-fill text-mode-hook-identify time-stamp-customization-hook)))
 '(transient-mark-mode t)
 '(version-control t)
 '(visible-bell t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
;; }}}

;; package load path
;; {{{
(funcall
 '(lambda ()
    (let ((old-dir default-directory))
      (cd "~/.emacs.d/lisp")
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path)
      (cd old-dir))))
;; }}}

;; behavior under windowing system
;; {{{
(if window-system
    (progn
      (tool-bar-mode nil)
      (scroll-bar-mode 'right)
      (setq scroll-conservatively 300
            scroll-preserve-screen-position 1)
      (setq x-select-enable-clipboard t)
      (set-frame-font "Monaco")
      (require 'color-theme)
      (setq color-theme-load-all-themes nil)
      (eval-after-load "color-theme"
        '(progn
           (color-theme-initialize)
           (color-theme-ruby-blue)))
      (add-hook 'post-command-hook
       '(lambda ()
          (cond (buffer-read-only (setq cursor-type 'hbar))
                (overwrite-mode (setq cursor-type 'box))
                (t (setq cursor-type 'bar)))))))
;; }}}

;; answer yes-no question simply with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; customize time-stamp
;; {{{
(defun time-stamp-customization-hook ()
  (setq time-stamp-end "$"
        time-stamp-format "%3a, %02d %3b %:y %02H:%02M:%02S"
        time-stamp-line-limit 3000
        time-stamp-start "Last[- ]\\\(Updated\\\|updated\\\|Modified\\\|modified\\\):[ \\\t]+"))
;; }}}

;; org-mode
;; {{{
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; }}}

;; C indentation style
;; {{{
(add-hook 'c-mode-hook
 '(lambda()
    (c-set-style "k&r")))
;; }}}

;; C++ indentation style
;; {{{
(add-hook 'c++-mode-hook
 '(lambda()
    (c-set-style "stroustrup")))
;; }}}

;; compile single C++ file with g++ if there's no makefile
;; {{{
(add-hook 'c++-mode-hook
 '(lambda ()
    (unless (or (file-exists-p "makefile") (file-exists-p "Makefile"))
      (set (make-local-variable 'compile-command)
        (let ((file (file-name-nondirectory buffer-file-name)))
          (concat "g++ -g -o " (file-name-sans-extension file) " " file))))))
;; }}}

;;; installed packages

;; AucTex
;; {{{
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; }}}

;; autopair
;; {{{
(require 'autopair)
(autopair-global-mode)
;; }}}

;; CMake
;; {{{
(require 'cmake-mode)
(setq auto-mode-alist
  (append 
   '(("CMakeLists\\.txt\\'" . cmake-mode)
     ("\\.cmake\\'" . cmake-mode))
   auto-mode-alist))
;; }}}

;; CEDET
;; {{{
;;(load "~/.emacs.d/user-cedet-conf")
(require 'cedet)
(semantic-mode 1)
(require 'sourcepair)
(add-hook 'c-mode-common-hook
 '(lambda ()
    (define-key c-mode-base-map (kbd "M-o") 'sourcepair-load)))
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

;; header2
;; {{{
(require 'header2)
(add-hook 'write-file-hooks 'auto-update-file-header)
;;(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook 'auto-make-header)
(setq header-date-format "%a, %d %b %Y %H:%M:%S")
;; }}}

;; htmlize
;; {{{
(require 'htmlize)
(setq htmlize-output-type "inline-css")
;; }}}

;; msf-abbrev
;; {{{
(require 'msf-abbrev)
(global-msf-abbrev-mode t)
;; }}}

;; python-mode
;; {{{
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; }}}

;; session
;; {{{
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;; }}}

;; SLIME
;; {{{
;; (set-language-environment "utf-8")
(setq inferior-lisp-program
  (cond
    ((string= window-system "w32") "/opt/ccl/wx86cl -K utf-8")
    (t "/opt/ccl/lx86cl64 -K utf-8")))
(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))
;; }}}

;; Tuareg
;; {{{
(load "append-tuareg")
(load "custom-tuareg")
;; }}}

;;; home-brew stuff

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

;; move the line(s) spanned by the active region up/down (line transposing)
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
      (add-hook (make-variable-buffer-local 'after-save-hook)
       '(lambda ()
          (byte-compile-file user-init-file))))))
;; }}}

;;; init.el ends here

