;; global configuration

(global-linum-mode t)



;; Always ALWAYS use UTF-8
; from https://github.com/bodil/emacs.d/blob/master/init.elh
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;
;; migrated options from my vimrc

;Don't wrap text (makes windows ugly)
;set nowrap
(setq-default truncate-lines t)

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



;;;Need to be migrated from vimrc


;magic for completion at the : command line.
;set wildmenu
;set wildmode=list:longest
;set wildignore=*.o,*.fasl,*.class

;do case-insensitive searching
;set ignorecase

;When a search phrase has uppercase, don't be case insensitive
;set smartcase

;Show me when I'm in insert/overtype mode
;set showmode

;When a command is in progress, show it in the status bar 
;set showcmd

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

;"use incremental search"
;set incsearch

;"highlight searches
;set hls

;"always use unix line endings
;set fileformat=unix
;set fileformats=unix,dos

;"set backspace to erase over everything
;set bs=indent,eol,start

;"set wrap
;set wrap

;"show what we are working on
;set title

;"keep 1000 items in history
;set history=1000

;"keep more context when scrolling
;"set scrolloff=3
;set scrolloff=999

;"keep prompts quiter
;set shortmess=atI

;"don't beep
;set visualbell
;set t_vb=

;"Link unnamed register and OS clipboard:
;if $TMUX == ''
;  set clipboard+=unnamed
;endif

;" Enable use of the mouse for all modes
;if has('mouse')
;    set mouse=a
;endif
 
;"enable syntax highlight and other good stuff but not when using vimdiff
;if &diff
;    syntax off
;else
;    syntax enable
;endif

;"map a key for pasting text in the terminal as this is
;"*my* default way of running vim :)
;set pastetoggle=<F12>
 
;"The commandbar is 2 high
;set cmdheight=2

;set number

;"More natural splitting from
;"http://robots.thoughtbot.com/post/48275867281/vim-splits-move-faster-and-more-naturally

;set splitbelow
;set splitright


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

;"blantanly plagarized from https://github.com/scrooloose/vimfiles/blob/master/vimrc

;"statusline setup
;set statusline=%F "tail of the filename

;"display a warning if fileformat isnt unix
;set statusline+=%#warningmsg#
;set statusline+=%{&ff!='unix'?'['.&ff.']':''}
;set statusline+=%*

;"display a warning if file encoding isnt utf-8
;set statusline+=%#warningmsg#
;set statusline+=%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.']':''}
;set statusline+=%*

;set statusline+=%h "help file flag
;set statusline+=%y "filetype
;set statusline+=%r "read only flag
;set statusline+=%m "modified flag

;set statusline+=%{fugitive#statusline()}

;set statusline+=%= "left/right separator
;set statusline+=%c, "cursor column
;set statusline+=%l/%L "cursor line/total lines
;set statusline+=\ %P "percent through file
;set laststatus=2

;" Catch trailing whitespace
;set listchars=tab:>-,trail:·,eol:$

;" Use sane Regexes
;nnoremap / /\v
;vnoremap / /\v

;nmap <silent> <leader>s :set nolist!<CR>

;"toggle line numbers
;map <leader>3 :set number!<CR>

;"map <F9> to reformat entire file. this also saves the current cursor
;"position so you are right back where you started. 
;map <F9> :let save_cursor = getpos(".")<CR> gg=G <CR> :call setpos(".", save_cursor)<CR>:echo "Buffer reformatted"<CR>
