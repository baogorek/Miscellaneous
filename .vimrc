execute pathogen#infect()

set nocompatible
filetype indent plugin on
syntax on
set wildmenu
set showcmd
set hlsearch

set ignorecase
set smartcase
set backspace=indent,eol,start
set visualbell
set ruler

set shiftwidth=2
set softtabstop=2
set expandtab

map <Leader>s :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
map <Leader>a :SlimuxShellLast<CR>
map <Leader>k :SlimuxSendKeysLast<CR>
