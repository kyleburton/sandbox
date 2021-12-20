" ~/.vimrc

" [Preamble] {{{

filetype off
filetype plugin indent on
set nocompatible

" }}}
" [General] {{{

set autoread
set autowrite
set backspace=2
set colorcolumn=80
set history=1000
set laststatus=2
set linebreak
set list
set listchars=trail:.,tab:>.
set matchtime=3
set nonumber
set encoding=utf-8 nobomb
set ruler
set showmode
set showcmd
set title
set ttyfast
set undofile
set visualbell

" Don't highlight lines longer than 800 characters
set synmaxcol=800

" Timeout on key codes but not mappings
set notimeout
set ttimeout
set timeoutlen=200

set complete=.,w,b,u,t
set completeopt=longest,menuone,preview

" }}}
" [Tabs/Spaces/Wrapping] {{{

set tabstop=4
set shiftwidth=4
set expandtab
set wrap
set formatoptions=qrn1

" }}}
" [Backups] {{{

set nobackup
set noswapfile

set undodir=~/.vim/tmp/undo/
set backupdir=~/.vim/tmp/backup/
set directory=~/.vim/tmp/swap/

" Create directories, if they don't already exist
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif

" }}}
" [Leader] {{{

let mapleader = ","
let maplocalleader = "\\"

" }}}
" [Color Scheme] {{{

syntax on
set background=dark
colorscheme badwolf
let g:badwolf_tabline = 2
let g:badwolf_html_link_underline = 0

" }}}
" [Mappings] {{{

inoremap jk <esc>

nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel

nnoremap <silent> <Space> @=(foldlevel('.')?'za':'l')<CR>
vnoremap <Space> zf
nnoremap <CR> :noh<CR><CR>
nnoremap <Leader>w :w<CR>

set pastetoggle=<F2>

" }}}
" [Searching and movement] {{{

set smartcase
set incsearch
set showmatch
set hlsearch

set scrolloff=3
set sidescroll=1
set sidescrolloff=10

" }}}
" [Wildmenu completion] {{{

set wildmenu                            " better tab completion
set wildmode=list:longest,full          " better tab completion
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     execute 'normal! g`"zvzz' |
        \ endif
augroup END

" }}}
" [statusline] {{{

set statusline=%<\ %n:%f\ %m%r%y%=%-35.(line:\ %l\ of\ %L,\ col:\ %c%V\ (%P)%)

" }}}
" [netrw] {{{

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_dirhistmax = 0

" }}}
" [Auto Comment] {{{

augroup auto_comment
    au!
    au FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
augroup END

" }}}
" [Vim Files] {{{

augroup ft_vim
    au!
    au FileType vim setlocal foldmethod=marker
augroup END

" }}}
" [Bash Files] {{{

augroup ft_bash
    au!
    au BufNewFile,BufRead *.sh set ft=sh
    au FileType bash set shiftwidth=2
augroup END

" }}}
" [C Files] {{{

augroup ft_c
    au!
    au BufNewFile,BufRead *.c,*.h set ft=c
    au FileType c set shiftwidth=2
augroup END

" }}}
" [Go Files] {{{

augroup ft_go
    au!
    au BufNewFile,BufRead *.go set ft=go
    au FileType go set nolist
augroup END

" }}}
" [YAML Files] {{{

augroup ft_yaml
    au!
    au BufNewFile,BufRead *.yaml,*.yml set ft=yaml
    au FileType yaml set shiftwidth=2 tabstop=2
augroup END

" }}}
" [Perl files] {{{

augroup ft_perl
    au!
    au BufNewFile,BufRead *.pl,*.pm set ft=perl
    au FileType perl set shiftwidth=2
augroup END

" }}}
" [Ruby Files] {{{

augroup ft_ruby
    au!
    au BufNewFile,BufRead *.rb set ft=ruby
    au FileType ruby set shiftwidth=2
augroup END

" }}}
" [Javascript Files] {{{

augroup ft_javascript
    au!
    au BufNewFile,BufRead *.js set ft=javascript
    au FileType javascript set shiftwidth=2
augroup END

" }}}
" [JSON Files] {{{

augroup ft_json
    au!
    au BufNewFile,BufRead *.json set ft=json
    au FileType json set shiftwidth=2
augroup END

" }}}
" [Markdown Files] {{{

augroup ft_markdown
    au!
    au BufNewFile,BufRead *.md,*.m*down set ft=ghmarkdown
augroup END

" }}}
" [HTML Files] {{{
"
augroup ft_html
    au!
    au BufNewFile,BufRead *.html,*.htm set ft=html
    au FileType html set shiftwidth=2
augroup END

" }}}
" [Javascript Files] {{{

augroup ft_javascript
    au!
    au BufNewFile,BufRead *.js set ft=javascript
    au FileType javascript set shiftwidth=2
augroup END

" }}}
" [CSS Files] {{{

augroup ft_css
    au!
    au BufNewFile,BufRead *.css set ft=css
    au FileType css set shiftwidth=2
augroup END

" }}}
" [Handlebar Files] {{{

augroup ft_hbs
    au!
    au BufNewFile,BufRead *.hbs set ft=hbs
    au FileType hbs set shiftwidth=2
augroup END

" }}}
" [Stylus Files] {{{

augroup ft_stylus
    au!
    au BufNewFile,BufRead *.styl set ft=stylus
    au FileType stylus set shiftwidth=2
augroup END

" }}}
" [Terraform Files] {{{

augroup ft_terraform
    au!
    au BufNewFile,BufRead *.tf set ft=terraform
    au FileType terraform set shiftwidth=2
augroup END

" }}}
" [Erlang Files] {{{

augroup ft_erlang
    au!
    au BufNewFile,BufRead *.erl,*.hrl,*.es,*.escript,*.xrl,*.config setf erlang
    au FileType erlang set shiftwidth=4 tabstop=4
augroup END

set runtimepath^=~/.vim/pack/plugins/start/vim-erlang-compiler
set runtimepath^=~/.vim/pack/plugins/start/vim-erlang-omnicomplete
set runtimepath^=~/.vim/pack/plugins/start/vim-erlang-runtime
set runtimepath^=~/.vim/pack/plugins/start/vim-erlang-tags
set tags=tags;$HOME/.vim/tags/

" }}}
" [Elixir Files] {{{

augroup ft_elixir
    au!
    au BufNewFile,BufRead *.ex,*.exs,*.eex setf elixir
    au FileType elixir set shiftwidth=4 tabstop=4
augroup END

" }}}

