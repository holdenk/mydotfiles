;; For hi-res
(set-face-attribute 'default nil :height 140)
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
(add-hook 'scala-mode-hook (function (lambda ()
				       (require 'whitespace)
				       (local-set-key (kbd "RET") 'newline-and-indent)
				       (make-local-variable 'before-save-hook)
				       (add-hook 'before-save-hook 'whitespace-cleanup)
				       ;; trailing whitespace
				       (setq show-trailing-whitespace t)
				       (whitespace-mode f)
				       ;; TODO 4 space indents on new line
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
			     (setq show-trailing-whitespace t)
			     (whitespace-mode f)
)))
;; asciidoc is fun
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))
;; spelling is hard
(add-hook 'scala-mode-hook (lambda ()
			     (flyspell-prog-mode)))
(add-hook 'adoc-mode-hook (lambda ()
			     (flyspell-mode 1)))
;; Load ensime
(add-to-list 'load-path "~/ensime/elisp")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
