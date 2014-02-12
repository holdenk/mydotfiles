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
