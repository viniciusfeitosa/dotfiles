;; Add your custom functions here

;; (defun something
;;    (do-something))

(provide 'base-functions)

;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(global-unset-key "\C-z")
(global-set-key "\C-z" 'advertised-undo)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)

(defun delete-line-from-any-position ()
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

(global-set-key (kbd "C-c k") 'delete-line-from-any-position)

(defun get-point (symbol &optional arg)
 "get the point"
 (funcall symbol arg)
 (point)
)

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
   (save-excursion
     (let ((beg (get-point begin-of-thing 1))
    	 (end (get-point end-of-thing arg)))
       (copy-region-as-kill beg end)))
)

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe
    (lambda()
       (if (string= "shell-mode" major-mode)
         (progn (comint-next-prompt 25535) (yank))
       (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
    	nil
          (funcall pasteMe))
      (funcall pasteMe))
  ))

(defun copy-word (&optional arg)
 "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
)

(global-set-key (kbd "C-c w") 'copy-word)

(defun copy-line (&optional arg)
 "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (paste-to-mark arg)
)

(global-set-key (kbd "C-c l") 'copy-line)

(global-set-key (kbd "C-c f") 'rgrep)

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
(newline-and-indent))

(global-set-key (kbd "C-l") 'end-of-line-and-indented-new-line)

(setq linum-format "%4d \u2502 ")
