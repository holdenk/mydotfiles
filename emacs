(set-face-attribute 'default nil :height 180)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))
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
)
;; Load ensime
(add-to-list 'load-path "~/ensime")
(require 'ensime)

