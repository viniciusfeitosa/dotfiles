;;Load package-install sources
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(defvar my-packages
  '(;;;; Go shit
    go-mode
    go-eldoc
    go-autocomplete

        ;;;;;; Markdown
    markdown-mode
    
        ;;;;;; shell
    eshell

        ;;;;;; Javascript
    json-mode
    js2-mode
        ;;;;;; Env
    project-explorer
    smooth-scroll
    buffer-move
    window-number
    better-defaults
    ein
    elpy
    pydoc
    flycheck
    material-theme
    py-autopep8
    fiplr
    ace-jump-mode
    magit
    git-gutter
    yasnippet
    helm
    helm-projectile
    autopair
    neotree
    all-the-icons
    discover-my-major)
  "My packages!")

;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))
    
;;Load Go-specific language syntax
(defun go-mode-setup ()
  (go-eldoc-setup))

;; Load git-gutter global
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; Heml config
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; autopair config
(require 'autopair)
(autopair-global-mode)

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

(setq mac-command-modifier 'meta)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(setq fiplr-ignored-globs '((directories (".git" ".svn" "env"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" "*.pyc"))))

(global-set-key (kbd "C-x f") 'fiplr-find-file)

(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-l") 'end-of-line-and-indented-new-line)

(global-set-key (kbd "C-c <left>") 'tabbar-backward-tab)
(global-set-key (kbd "C-c <right>") 'tabbar-forward-tab)

(setq inhibit-startup-message t) ;; hide the startup message
;;(load-theme 'material t) ;; load material theme
(load-theme 'tango-dark t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;;
;; ace jump mode major function
;; 
(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)

(setq linum-format "%4d \u2502 ")


;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; PYTHON CONFIGURATION
;; --------------------------------------

(elpy-enable)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save

;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq elpy-rpc-backend "jedi")


;; GO  CONFIGURATION
;; --------------------------------------

(add-hook 'go-mode-hook 'go-mode-setup)

;;Format before saving
(defun go-mode-setup ()
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Goimports
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Godef, shows function definition when calling godef-jump
(defun go-mode-setup ()
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Custom Compile Command
(defun go-mode-setup ()
  (setq compile-command "go build -v && go test -v && go vet && golint && errcheck")
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'go-mode-setup)

;;Load auto-complete

(ac-config-default)
(require 'auto-complete-config)
;; auto-complete to Golang
(when (go-mode)
  (require 'go-autocomplete))

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;Project Explorer
(require 'project-explorer)
(global-set-key (kbd "M-e") 'project-explorer-toggle)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (js2-mode react-snippets django-snippets yasnippet window-number buffer-move smooth-scroll project-explorer json-mode markdown-mode go-autocomplete go-eldoc go-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
