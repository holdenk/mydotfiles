;; No startup screen

(setq inhibit-startup-screen t)

;; Enable column-number-mode

(setq column-number-mode t)

;; For hi-res
(set-face-attribute 'default nil :height 120)
;; Turn on debug on quit
(setq debug-on-quit 't)
;; Load packages
(require 'package)
(require 'cl)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))
(unless (package-installed-p 'adoc-mode)
  (package-refresh-contents) (package-install 'adoc-mode))
(unless (package-installed-p 'ensime)
  (package-refresh-contents) (package-install 'ensime))
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
				       (add-hook 'before-save-hook 'whitespace-cleanup)
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
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/ess")
(load "ess-site")
