set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" Plugin 'fatih/vim-go'
Plugin 'klen/python-mode'
Plugin 'wting/rust.vim'
Plugin 'chemzqm/vim-jsx-improve'
call vundle#end()
filetype plugin indent on

set backspace=indent,eol,start
set number " Show line numbers

syntax on

" Folding *********************************************************************

set foldenable
set foldmethod=syntax
set foldlevelstart=99

" Tabs ************************************************************************
" Use 2 spaces
set softtabstop=2
set shiftwidth=2
set tabstop=2
set expandtab

" Searching *******************************************************************
set incsearch  " incremental search, search as you type

" Status Line *****************************************************************
set showcmd
set ruler " Show ruler

" Directories *****************************************************************
" Setup backup location and enable
set backupdir=~/.vim/backup
set backup

" Set Swap directory
set directory=~/.vim/backup/swap

set runtimepath+=$GOROOT/misc/vim

"let g:pymode_python = 'python3'

let g:pymode_rope_lookup_project = 0
let g:pymode_rope = 0
let g:pymode_options_colorcolumn = 0
" set colorcolumn=""
let g:pymode_lint_options_pep8 =
      \ {'max_line_length': 140}

cabbrev E Explore

au BufRead,BufNewFile Jenkinsfile* setfiletype groovy

" http://stackoverflow.com/questions/356126/how-can-you-automatically-remove-trailing-whitespace-in-vim
" automatically remove trailing whitespace
fun! <SID>StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun

" autocmd FileType c,cpp,java,php,ruby,python,rust.rs autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()


