;; global configuration

(global-linum-mode t)



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

;Don't wrap text (makes windows ugly)
;set nowrap
(setq-default truncate-lines t)


;;;Need to be migrated from vimrc

;expand tabs to spaces
;set expandtab

;"Use industry standard 4-char tabs
;set tabstop=4 

;"Use standard 4-char indentation
;set shiftwidth=4 

;set softtabstop=4

;"Indent/Dedent to nearest 4-char boundary
;set shiftround 

;"indent in a smart way, instead of returning to the left margin all the time
;set autoindent 

;"highlight searches
;set hls

;"always use unix line endings
;set fileformat=unix
;set fileformats=unix,dos

;" Enable use of the mouse for all modes
;if has('mouse')
;    set mouse=a
;endif
 

;if has("autocmd")
;  filetype plugin indent on
;  " Load matchit (% to bounce from do to end, etc.)
;  runtime! macros/matchit.vim
;  augroup myfiletypes
;    " Clear old autocmds in group
;    autocmd!
;    " autoindent with two spaces, always expand tabs
;    autocmd FileType ruby,eruby,yaml,javascript set ai sw=2 sts=2 et
;    autocmd FileType snippets set noexpandtab
;    autocmd bufwritepost .vimrc source $MYVIMRC
;    "set json to to javascript
;    autocmd BufRead *.json set filetype=javascript
;    "set to go
;    autocmd BufRead *.go set filetype=go
;    "Gemfile, Capfile are ruby files.
;    autocmd BufRead Gemfile,Capfile set filetype=ruby
;    autocmd BufNewFile,BufRead *.md,*.markdown setlocal spell spelllang=en_us
;    autocmd BufRead COMMIT_EDITMSG setlocal spell spelllang=en_us
;  augroup END
;endif

;" Catch trailing whitespace
;set listchars=tab:>-,trail:·,eol:$

;nmap <silent> <leader>s :set nolist!<CR>

;"toggle line numbers
;map <leader>3 :set number!<CR>

;"map <F9> to reformat entire file. this also saves the current cursor
;"position so you are right back where you started. 
;map <F9> :let save_cursor = getpos(".")<CR> gg=G <CR> :call setpos(".", save_cursor)<CR>:echo "Buffer reformatted"<CR>
