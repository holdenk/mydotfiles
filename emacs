;;; emacs --- Holden's Emacs Configuration
;;; Commentary:
;;;
;;; This is my personal Emacs configuration optimized for Scala, Java, Python,
;;; and general development work. It includes LSP support, Magit for Git,
;;; GitHub Copilot integration, and various language-specific modes.
;;;
;;; Key Features:
;;; - LSP Mode with support for Scala (Metals), Java, Python, and more
;;; - Magit for Git integration
;;; - GitHub Copilot for AI-assisted coding
;;; - Company mode for auto-completion
;;; - EditorConfig support for consistent coding styles
;;; - Language-specific configurations for Scala, Java, Python, Rust, Go, etc.
;;;
;;; Platform Support:
;;; - Linux (primary)
;;; - macOS (tested and compatible)
;;;
;;; Requirements:
;;; - Emacs 27.1+ (tested with 31.0+)
;;; - Internet connection for initial package installation
;;; - For Scala: Metals language server
;;; - For Java: Eclipse JDT Language Server (installed via lsp-java)
;;; - For Python: Pyright language server
;;;
;;; Installation:
;;; 1. Copy this file to ~/.emacs or ~/.emacs.d/init.el
;;; 2. Start Emacs - packages will be installed automatically
;;; 3. Install language servers as needed (e.g., metals for Scala)
;;;
;;; For detailed key bindings, see README_EMACS.md
;;;
;;; Author: Holden Karau
;;; Created: Earlier than git remembers
;;; Updated: 2026-01-20
;;; URL: https://github.com/holdenk/mydotfiles
;;;
;;; Code:

;;; ============================================================================
;;; Basic UI Configuration
;;; ============================================================================

;; No startup screen - get straight to work
(setq inhibit-startup-screen t)

;; Enable column-number-mode - show column number in mode line
(setq column-number-mode t)

;; Font size for hi-res displays
;; Adjust this value based on your display (230 works well for 4K)
(set-face-attribute 'default nil :height 230)

;; Turn on debug on quit - useful for troubleshooting
(setq debug-on-quit 't)

;;; ============================================================================
;;; Package Management Setup
;;; ============================================================================

;; Load package system
(require 'package)

;; Use cl-lib instead of deprecated cl package for modern Emacs compatibility
(require 'cl-lib)

;; Configure package archives
;; Using HTTPS for security
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; Initialize package system
(unless package--initialized (package-initialize))

;; Refresh package contents if not already cached
(unless package-archive-contents (package-refresh-contents))

;; List of packages to install automatically
;; Core packages for development workflow
(setq package-list '(
  ;; Version control
  magit                    ; Git interface
  
  ;; File navigation
  find-things-fast        ; Fast file finding
  
  ;; Language modes
  adoc-mode               ; AsciiDoc editing
  go-mode                 ; Go language support
  scala-mode              ; Scala language support
  sbt-mode                ; Scala Build Tool integration
  rust-mode               ; Rust language support
  typescript-mode         ; TypeScript support
  jsonnet-mode            ; Jsonnet configuration language
  yaml-mode               ; YAML editing
  markdown-mode           ; Markdown editing
  dockerfile-mode         ; Dockerfile syntax
  
  ;; Language servers and completion
  use-package             ; Package configuration macro
  lsp-mode                ; Language Server Protocol client
  lsp-ui                  ; UI enhancements for LSP
  lsp-java                ; Java language server
  lsp-latex               ; LaTeX language server
  lsp-pyright             ; Python language server (Pyright)
  company                 ; Auto-completion framework
  
  ;; Code quality
  flycheck                ; Syntax checking
  yasnippet               ; Snippet expansion
  
  ;; GitHub Copilot
  quelpa-use-package      ; Install packages from source
  
  ;; Editor enhancements
  editorconfig            ; EditorConfig support
  cider                   ; Clojure IDE
  transient               ; Transient command interface (for Magit)
))

;; Install any missing packages automatically
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Always ensure packages are installed when using use-package
(setq use-package-always-ensure t)

;;; ============================================================================
;;; Performance Tuning
;;; ============================================================================

;; LSP/Metals performance optimizations
;; Increase process output buffer size for better LSP performance
(setq read-process-output-max (* 1024 1024))  ; 1MB (default is 4KB)

;; Increase garbage collection threshold during startup
;; This speeds up initialization significantly
(setq gc-cons-threshold (* 100 1024 1024))    ; 100MB (default is 800KB)

;;; ============================================================================
;;; Shell Script Mode Configuration
;;; ============================================================================

;; Shell script indentation settings
(add-hook 'sh-mode-hook
	  (function (lambda ()
		      (setq sh-basic-offset 2
			    sh-indentation 2))))

;;; ============================================================================
;;; Scala Development Configuration
;;; ============================================================================
;;; Scala support includes:
;;; - scala-mode for syntax highlighting and basic editing
;;; - sbt-mode for SBT integration
;;; - lsp-mode/lsp-metals for intelligent code completion and navigation
;;;
;;; Key Bindings (Scala):
;;; - M-d: Find definition (with sbt-find-definitions or lsp-find-definition)
;;; - M-.: Navigate to definition (LSP)
;;; - C-x ': Re-run previous SBT command
;;; ============================================================================
;; Enable scala-mode for .scala and .sbt files
(use-package scala-mode :mode "\\.s\\(cala\\|bt\\)$")

;; Enable sbt-mode for SBT integration
(use-package sbt-mode)

;; Configure LSP mode with Scala support
(use-package lsp-mode
  :hook ((scala-mode . lsp))
  :custom (lsp-completion-provider :capf))

;; Metals (Scala language server) configuration
(use-package lsp-metals
  :after lsp-mode
  :hook (scala-mode . lsp)
  :custom
  ;; Show tree view when views are received
  (lsp-metals-treeview-show-when-views-received t)
  ;; Increase file watch threshold for large projects
  (lsp-file-watch-threshold 200000))

;; Scala mode key bindings
(add-hook 'scala-mode-hook '(lambda ()
   ;; sbt-find-definitions: Find definitions using grep
   (local-set-key (kbd "M-d") 'sbt-find-definitions)

   ;; Re-compile code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)

   ;; LSP-based definition lookup (better when metals is working)
   (local-set-key (kbd "M-.") #'lsp-find-definition)
))

;; Scala indentation style
(setq scala-indent:use-javadoc-style t)

;; Additional Scala mode hooks for formatting and indentation
(add-hook 'scala-mode-hook (function (lambda ()
				       ;; Auto-indent on return
				       (local-set-key (kbd "RET") 'newline-and-indent)
				       ;; Clean up whitespace before saving
				       (make-local-variable 'before-save-hook)
				       (add-hook 'before-save-hook 'whitespace-cleanup nil t)
				       ;; Indentation settings
				       (c-set-offset 'arglist-intro 4)
				       ;; Better RET handling for Scala
				       (local-set-key (kbd "RET")
						      '(lambda ()
							 (interactive)
							 (newline-and-indent)
							 (scala-indent:insert-asterisk-on-multiline-comment)))
				       )))

;;; ============================================================================
;;; General Editor Settings
;;; ============================================================================

;; Enable transient mark mode - highlight selected regions
(transient-mark-mode 1)

;;; ============================================================================
;;; Java Development Configuration
;;; ============================================================================
;;; Java support includes:
;;; - Built-in java-mode for syntax highlighting
;;; - lsp-java for language server support (Eclipse JDT)
;;;
;;; Key Bindings (Java):
;;; - M-.: Navigate to definition (LSP)
;;; - RET: Auto-indent on newline
;;; ============================================================================
;; Java LSP key binding
(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "M-.") #'lsp-find-definition)))

;; Java mode formatting and indentation
(add-hook 'java-mode-hook (function
			   (lambda ()
			     ;; Auto-indent on return
			     (local-set-key (kbd "RET") 'newline-and-indent)
			     ;; Indentation settings
			     (setq c-basic-offset 2
				   tab-width 2
				   indent-tabs-mode nil      ; Use spaces, not tabs
				   c-argdecl-indent 0          ; No extra indent for arguments
				   c-tab-always-indent t
				   backward-delete-function nil)
			     (c-set-offset 'arglist-intro '+)
)))

;; Enable LSP for Java files
(add-hook 'java-mode-hook #'lsp)

;;; ============================================================================
;;; Documentation and Markup Modes
;;; ============================================================================

;; AsciiDoc mode for .asciidoc files
(add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode))

;; Enable spell checking in various modes
;; flyspell-prog-mode: spell check in comments and strings only
(add-hook 'scala-mode-hook (lambda ()
			     (flyspell-prog-mode)))

;; flyspell-mode: spell check everything (for documentation files)
(add-hook 'adoc-mode-hook (lambda ()
			     (flyspell-mode 1)))

;;; ============================================================================
;;; Auto-completion Configuration (Company Mode)
;;; ============================================================================

;; Load company mode for auto-completion
(require 'company)

;; Customize company-mode key bindings
;; Disable default bindings that might conflict with other modes
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "<up>") nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map [return] nil)
(define-key company-active-map [tab] nil)
(define-key company-active-map [space] nil)
(define-key company-active-map (kbd "TAB") nil)

;; Set up better company-mode navigation
;; C-<down>: Next completion
;; C-<up>: Previous completion
;; C-<return>: Accept completion
(define-key company-active-map (kbd "C-<down>") 'company-select-next)
(define-key company-active-map (kbd "C-<up>") 'company-select-previous)
(define-key company-active-map (kbd "<C-return>") 'company-complete-selection)

;; Enable company-mode globally
(global-company-mode)

;;; ============================================================================
;;; Clojure Development Configuration
;;; ============================================================================

;; CIDER (Clojure IDE) configuration
(require 'cider)

;; Enable eldoc in CIDER (show function signatures)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Enable subword-mode for better navigation in camelCase/snake_case
(add-hook 'cider-mode-hook 'subword-mode)

;; Enable CIDER in Clojure mode
(add-hook 'clojure-mode-hook 'cider-mode)

;; Prefer local resources (faster REPL startup)
(set 'cider-prefer-local-resources t)

;;; ============================================================================
;;; File Navigation
;;; ============================================================================

;; find-things-fast configuration
;; C-x t: Fast file finder
(global-set-key (kbd "C-x t") 'ftf-find-file)
(setq ftf-filetypes '("*"))  ; Allow all file types
(setq ftf-ignored-directories '("connector" ".git" "target"))

;; Project.el file finder
;; C-x p: Find file in project
(global-set-key (kbd "C-x p") 'project-find-file)

;;; ============================================================================
;;; Remote File Editing (TRAMP)
;;; ============================================================================

;; Use SSH as default method for remote file access
(setq tramp-default-method "ssh")

;;; ============================================================================
;;; Visual Customizations
;;; ============================================================================

;; Make selection highlighting more visible
(set-face-attribute 'region nil :background "#999")

;;; ============================================================================
;;; LSP Mode Additional Configuration
;;; ============================================================================

;; Configure LSP mode for various languages
(use-package lsp-mode
  ;; Enable lsp for scala
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))  ; Use flycheck instead of flymake

;; LSP UI enhancements
(use-package lsp-ui)

;; Additional LSP key bindings for Scala
(add-hook 'scala-mode-hook
	  '(lambda ()
	     (local-set-key "\M-d" 'lsp-find-definition)
	     ))

;; Additional LSP key bindings for Java
(add-hook 'java-mode-hook
	  '(lambda ()
	     (local-set-key "\M-d" 'lsp-find-definition)
	     ))

;; Enable LSP for XML files
(add-hook 'nxml-mode-hook #'lsp)

;;; ============================================================================
;;; Rust Development Configuration
;;; ============================================================================

;; Enable rust-mode for Rust development
(use-package rust-mode)

;;; ============================================================================
;;; Git Integration (Magit)
;;; ============================================================================

;; Better process handling for Magit
(setq magit-process-connection-type t)

;; Magit key binding
;; C-x g: Open Magit status
(use-package transient)
(use-package magit :after transient :bind ("C-x g" . magit-status))

;;; ============================================================================
;;; GitHub Copilot Configuration
;;; ============================================================================
;;; GitHub Copilot provides AI-assisted code completion
;;;
;;; Key Bindings (Copilot):
;;; - M-<next> (M-PageDown): Next completion suggestion
;;; - M-<prior> (M-PageUp): Previous completion suggestion  
;;; - M-<right>: Accept completion by word
;;; - M-<down>: Accept completion by line
;;; ============================================================================
;; Install Copilot from GitHub using quelpa
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

;;; ============================================================================
;;; EditorConfig Support
;;; ============================================================================

;; Enable EditorConfig for consistent coding styles across editors
(use-package editorconfig :config (editorconfig-mode 1))

;;; ============================================================================
;;; Custom Variables
;;; ============================================================================
;;; This section is auto-generated by Emacs Custom interface
;;; It's safe to edit manually, but be careful with the format

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-pyright lsp-latex lsp-mssql 0blayout editorconfig copilot.el
		 quelpa-use-package typescript-mode cargo-mode
		 flycheck-rust ## rust-mode cider company lsp-java
		 dockerfile-mode yasnippet yaml-mode sbt-mode lsp-ui
		 lsp-mode use-package jsonnet-mode flycheck go-mode
		 adoc-mode find-things-fast magit scala-mode
		 cmake-mode))
 '(warning-suppress-types '((emacs))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; ============================================================================
;;; Platform-Specific Configuration
;;; ============================================================================

;; macOS-specific settings
(when (eq system-type 'darwin)
  ;; Use command key as meta on macOS
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  
  ;; Better font rendering on macOS
  (setq ns-use-thin-smoothing t)
  
  ;; macOS uses different path for executables
  ;; Add common Homebrew paths for language servers
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/opt/homebrew/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin" "/opt/homebrew/bin"))))

;;; emacs ends here
