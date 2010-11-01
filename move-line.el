;;; move-line.el --- move the current line up/down.
;; (C) Copyright 2010, Ji Han (jihan917<at>yahoo<dot>com).


(defun move-line-up (n)
  "move the current line up by N lines."
  (interactive "*p")
  (let ((lnum (line-number-at-pos))
        (col (current-column)))
    (kill-whole-line)
    (forward-line (- (or n 1)))
    (yank)
    (forward-line -1)
    (move-to-column col)))

(defun move-line-down (n)
  "move the current line down by N lines."
  (interactive "*p")
  (let ((lnum (line-number-at-pos))
        (col (current-column)))
    (kill-whole-line)
    (forward-line (or n 1))
    (yank)
    (forward-line -1)
    (move-to-column col)))


(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; move-line.el ends here
