;;; move-lines.el --- move the line(s) spanned by the active region up/down.
;;
;; Update: if there's no active region, or there's no region at all,
;; move-lines would operate on the current line.
;;
;; Update: care has been taken to restore the mark if it's within the moved lines.
;; also, the highlight on the active region is preserved.
;;
;; (C) Copyright 2010, Ji Han (jihan917<at>yahoo<dot>com).


;; move the line(s) spanned by the active region up/down.
;; {{{
(defun move-lines (n)
  (let ((beg) (end) (keep))
    (if mark-active 
        (save-excursion
          (setq keep t)
          (setq beg (region-beginning)
                end (region-end))
          (goto-char beg)
          (setq beg (line-beginning-position))
          (goto-char end)
          (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (let ((offset (if (and (mark t) 
                           (and (>= (mark t) beg)
                                (< (mark t) end)))
                      (- (point) (mark t))))
          (rewind (- end (point))))
      (goto-char (if (< n 0) beg end))
      (forward-line n)
      (insert (delete-and-extract-region beg end))
      (backward-char rewind)
      (if offset (set-mark (- (point) offset))))
    (if keep
        (setq mark-active t
              deactivate-mark nil))))

(defun move-lines-up (n)
  "move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (move-lines (- (or n 1))))

(defun move-lines-down (n)
  "move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (move-lines (or n 1)))
;; }}}

;; move-lines.el ends here
