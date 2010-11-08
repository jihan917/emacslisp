;;; user-keymaps.el

(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-:"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "C-x C-l"))
(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-x f"))


(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)
(global-set-key (kbd "C-2") 'set-mark-command)


(defun cc-set-shortcut-keys ()
  (local-set-key (kbd "C-S-B") 'compile)
  (local-set-key (kbd "<f5>") 'gdb))

(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'cc-set-shortcut-keys))


;; user-keymaps.el ends here
