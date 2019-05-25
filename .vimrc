execute pathogen#infect()

set nocompatible
filetype indent plugin on
syntax on
set wildmenu
set showcmd
set hlsearch

color desert

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

map <Leader>c :SlimuxShellRun cpaste<CR>
map <Leader>e :SlimuxShellRun --<CR>
map <Leader>p :SlimuxShellConfigure<CR>
