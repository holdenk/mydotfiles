;; No startup screen

(setq inhibit-startup-screen t)

;; Enable column-number-mode

(setq column-number-mode t)

;; For hi-res
(set-face-attribute 'default nil :height 170)
;; Turn on debug on quit
(setq debug-on-quit 't)
;; Load packages
(require 'package)
(require 'cl)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
;;(package-initialize)

;;(when (not package-archive-contents)
;;  (package-refresh-contents))
;;(setq package-list '(ensime magit find-things-fast scala-mode2 adoc-mode))
;;(setq package-list '(magit find-things-fast adoc-mode ensime))
(setq package-list '(magit find-things-fast adoc-mode go-mode flycheck jsonnet-mode use-package lsp-mode lsp-ui sbt-mode yaml-mode yasnippet markdown-mode dockerfile-mode lsp-java rust-mode typescript-mode  quelpa-use-package editorconfig))
;;(package-initialize)
;; Fetch package list
;;(unless package-archive-contents
;;  (package-refresh-contents))
;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
;; Shell Hook
(add-hook 'sh-mode-hook
	  (function (lambda ()
		      (setq sh-basic-offset 2
			    sh-indentation 2))))
;; Load sbt mode
(unless (package-installed-p 'sbt-mode)
  (package-refresh-contents) (package-install 'sbt-mode))
(add-hook 'scala-mode-hook '(lambda ()
   ;; sbt-find-definitions is a command that tries to find (with grep)
   ;; the definition of the thing at point.
   (local-set-key (kbd "M-.") 'sbt-find-definitions)

   ;; use sbt-run-previous-command to re-compile your code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
))
;; scala indents
(setq scala-indent:use-javadoc-style t)
(add-hook 'scala-mode-hook (function (lambda ()
				       (local-set-key (kbd "RET") 'newline-and-indent)
				       (make-local-variable 'before-save-hook)
				       (add-hook 'before-save-hook 'whitespace-cleanup nil t)
				       ;; trailing whitespace
				       (c-set-offset 'arglist-intro 4)
				       (local-set-key (kbd "RET")
						      '(lambda ()
							 (interactive)
							 (newline-and-indent)
							 (scala-indent:insert-asterisk-on-multiline-comment)))
				       )))
;; Enable transient mark mode
(transient-mark-mode 1)
;; Java mode settings
(add-hook 'java-mode-hook (function
			   (lambda ()
			     (local-set-key (kbd "RET") 'newline-and-indent)
			     ;; indentation
			     (setq c-basic-offset 2
				   tab-width 2
				   indent-tabs-mode nil
				   c-argdecl-indent 0       ; Do not indent argument decl's extra
				   c-tab-always-indent t
				   backward-delete-function nil)
			     (c-set-offset 'arglist-intro '+)
			     ;; trailing whitespace
			     ;;(setq show-trailing-whitespace t)
			     ;;(whitespace-mode 'f)
)))
;; asciidoc is fun
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))
;; spelling is hard
(add-hook 'scala-mode-hook (lambda ()
			     (flyspell-prog-mode)))
(add-hook 'adoc-mode-hook (lambda ()
			     (flyspell-mode 1)))
;; Load ensime
;;(require 'ensime)
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; clojure crap
(unless (package-installed-p 'company)
  (package-refresh-contents) (package-install 'company))
(unless (package-installed-p 'magit)
  (package-refresh-contents) (package-install 'magit))
(unless (package-installed-p 'cider)
  (package-refresh-contents) (package-install 'cider))
(require 'company)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(require 'cider)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "<up>") nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map [return] nil)
(define-key company-active-map [tab] nil)
(define-key company-active-map [space] nil)
(define-key company-active-map (kbd "TAB") nil)
(define-key company-active-map (kbd "C-<down>") 'company-select-next)
(define-key company-active-map (kbd "C-<up>") 'company-select-previous)
(define-key company-active-map (kbd "<C-return>") 'company-complete-selection)
(global-company-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'cider-mode)
(set 'cider-prefer-local-resources t)
;; ess (R and crap)
;;(add-to-list 'load-path "/usr/share/emacs24/site-lisp/ess")
;;(load "ess-site")

(global-set-key (kbd "C-x t") 'ftf-find-file) ; bind to C-x t
(setq ftf-filetypes '("*"))                   ; allow all filetypes
;; Tramp mode
(setq tramp-default-method "ssh")
;; Highlight wasn't super visible on this machine
(set-face-attribute 'region nil :background "#999")
(use-package lsp-mode
  ;; Enable lsp for scala
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))
(use-package lsp-ui)
(add-hook 'scala-mode-hook
	  '(lambda ()
	     (local-set-key "\M-d" 'lsp-find-definition)
	     ))
(add-hook 'java-mode-hook
	  '(lambda ()
	     (local-set-key "\M-d" 'lsp-find-definition)
	     ))
(add-hook 'java-mode-hook #'lsp)

;; Magit over tramp hacks?
(setq magit-process-connection-type t)
(global-set-key (kbd "C-x p") 'project-find-file)
;; Sup?

(add-hook 'nxml-mode-hook #'lsp)

(setq magit-process-connection-type t)
(global-set-key (kbd "C-x p") 'project-find-file)

(require 'rust-mode)

(require 'rust-mode)

(require 'quelpa-use-package)

(use-package copilot
  :quelpa (copilot.el :fetcher github
                      :repo "zerolfx/copilot.el"
                      :branch "main"
                      :files ("dist" "*.el")))
