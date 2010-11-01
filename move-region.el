;;; move-region.el --- move those lines that the region spans up/down.
;; (if the region is inactive, the current line is assumed.)
;; (C) Copyright 2010, Ji Han (jihan917<at>yahoo<dot>com).


(defun move-region (n)
  (let ((beg) (end) (rwd))
    (if mark-active
      (save-excursion
        (setq beg (region-beginning)
              end (region-end))
        (goto-char beg)
        (setq beg (line-beginning-position))
        (goto-char end)
        (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (setq rwd (- end (point)))
    (goto-char (if (< n 0) beg end))
    (forward-line n)
    (insert (delete-and-extract-region beg end))
    (backward-char rwd)))

(defun move-region-up (n)
  "move the region up by N lines."
  (interactive "*p")
  (move-region (- (or n 1))))

(defun move-region-down (n)
  "move the region down by N lines."
  (interactive "*p")
  (move-region (or n 1)))


(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)

;; move-region.el ends here