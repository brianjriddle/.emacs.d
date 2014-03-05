;; global configuration

;;(global-linum-mode 0)



;; Always ALWAYS use UTF-8
; from https://github.com/bodil/emacs.d/blob/master/init.elh
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; taken from http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


;; from emacs starter-kit https://github.com/technomancy/emacs-starter-kit

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(color-theme-solarized
                      expand-region
                      feature-mode
                      git-commit-mode
                      markdown-mode
                      php-mode
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

;;configure modes per file type

(autoload 'feature-mode "feature-mode" "Major mode for editing cucumber/gherkin files" t)
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

(autoload 'markdown-mode "markdown-mode" "Major mode for markdown" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(autoload 'yaml-mode "yaml-mode" "Major mode for yaml" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(autoload 'php-mode "php-mode" "Major mode for php" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;;; fix copy & paste when running in terminal on osx
;; from
;; http://allkindsofrandomstuff.blogspot.se/2009/09/sharing-mac-clipboard-with-emacs.html


(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil)) 
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;;;key bindings

(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;;;
;; migrated options from my vimrc

(setq-default truncate-lines t)
