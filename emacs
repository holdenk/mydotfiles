;; No startup screen

(setq inhibit-startup-screen t)

;; Enable column-number-mode

(setq column-number-mode t)

;; For hi-res
;; (set-face-attribute 'default nil :height 230)
(defun holden/set-font-height-dynamic ()
  "Set font height based on display pixel size (fallback-friendly)."
  (let* ((pxw (display-pixel-width))
         ;; rough tiers; tweak numbers to taste
         (h (cond
             ((>= pxw 3800) 220) ; 4K-ish
             ((>= pxw 2500) 190) ; 1440p-ish
             ((>= pxw 1900) 160) ; 1080p-ish
             (t 140))))
    (set-face-attribute 'default nil :height h)
    (message "Font height set to %d (display width %dpx)" h pxw)))
;; Enabled the high res switching
(add-hook 'after-init-hook #'holden/set-font-height-dynamic)
(add-hook 'after-make-frame-functions (lambda (_frame) (holden/set-font-height-dynamic)))
;; Turn on debug on quit
(setq debug-on-quit 't)
;; Load packages
(require 'package)
(require 'cl-lib)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(unless package--initialized (package-initialize))
(unless package-archive-contents (package-refresh-contents))

(setq package-list '(magit find-things-fast adoc-mode go-mode flycheck jsonnet-mode use-package lsp-mode lsp-latex lsp-pyright lsp-ui sbt-mode yaml-mode yasnippet markdown-mode dockerfile-mode lsp-java rust-mode typescript-mode  quelpa-use-package editorconfig scala-mode sbt-mode company cider dap-mode))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
(setq use-package-always-ensure t)


;; Perf knobs LSP/Metals likes
(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq gc-cons-threshold (* 100 1024 1024))   ; 100MB

;; Shell Hook
(add-hook 'sh-mode-hook
	  (function (lambda ()
		      (setq sh-basic-offset 2
			    sh-indentation 2))))

;; Scala stuff
(use-package scala-mode :mode "\\.s\\(cala\\|bt\\)$")
(use-package sbt-mode)
(use-package lsp-mode
  :hook ((scala-mode . lsp))
  :custom (lsp-completion-provider :capf))
(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :config (dap-auto-configure-mode))
(use-package lsp-metals
  :after lsp-mode
  :hook (scala-mode . lsp)
  :custom
  (lsp-metals-treeview-show-when-views-received t)
  (lsp-file-watch-threshold 200000)) ;; maybe too much?
(add-hook 'scala-mode-hook '(lambda ()
   ;; sbt-find-definitions is a command that tries to find (with grep)
   ;; the definition of the thing at point.
   (local-set-key (kbd "M-d") 'sbt-find-definitions)

   ;; use sbt-run-previous-command to re-compile your code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)

   ;; lsp mode, much better if metals is working
   (local-set-key (kbd "M-.") #'lsp-find-definition)
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
(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "M-.") #'lsp-find-definition))
)
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
(require 'company)

;; magic
(use-package transient)
(use-package magit :after transient :bind ("C-x g" . magit-status))
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
(setq ftf-ignored-directories '("connector" ".git" "target"))
;; Tramp mode
(setq tramp-default-method "ssh")
;; Highlight wasn't super visible on this machine
(set-face-attribute 'region nil :background "#999")
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
(add-hook 'nxml-mode-hook #'lsp)

(use-package rust-mode)
(setq magit-process-connection-type t)
(global-set-key (kbd "C-x p") 'project-find-file)

;; Copilot via quelpa
(use-package quelpa-use-package)
(use-package copilot
  :quelpa (copilot.el :fetcher github
                      :repo "zerolfx/copilot.el"
                      :branch "main"
                      :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-mode-map (kbd "M-<next>")  #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-<prior>") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-<right>") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "M-<down>")  #'copilot-accept-completion-by-line))

(use-package editorconfig :config (editorconfig-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(## adoc-mode cider company copilot.el dap dockerfile-mode
	find-things-fast flycheck go-mode jsonnet-mode lsp-java
	lsp-latex lsp-pyright lsp-ui magit quelpa-use-package
	rust-mode sbt-mode scala-mode typescript-mode yaml-mode
	yasnippet))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
