if &compatible
  set nocompatible
endif

set runtimepath+=/home/skrat/.cache/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('/home/skrat/.cache/dein')
  call dein#begin('/home/skrat/.cache/dein')

  " Let dein manage dein
  " Required:
  call dein#add('/home/skrat/.cache/dein/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here:
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('nanotech/jellybeans.vim')
  call dein#add('guns/vim-clojure-static')

  " You can specify revision/branch/tag.
  call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable
syntax on

if dein#check_install()
  call dein#install()
endif

colorscheme jellybeans

set et
set hidden
set hlsearch
set invnumber
set nobackup
set smartindent
set smarttab
set wildignorecase
set incsearch
set hlsearch

autocmd BufEnter * setlocal cursorline

if (v:version >= 730)
    set history=1000
    set undodir=~/.vim/undo
    set undofile
    set undolevels=1000
    set undoreload=10000
    set ttymouse=xterm2
endif

let mapleader = " "

nnoremap <leader>w :w<cr>
nnoremap <leader>wq :wq<cr>
nnoremap <leader>q :qall<cr>
nnoremap <leader><tab> :bn<cr>
