(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


;; from emacs starter-kit https://github.com/technomancy/emacs-starter-kit

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(color-theme-solarized
                      starter-kit 
                      starter-kit-js 
                      starter-kit-ruby 
                      starter-kit-bindings 
                      yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; configure solarized

(load-theme 'solarized-dark t)



;;configure yaml-mode

(require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
