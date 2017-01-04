set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set smarttab
set nowrap


filetype plugin indent on

" open split panes to the bottom right 
set splitbelow
set splitright

" Plugin init
set nocompatible
set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim


" -------------- Plugins ---------------
" --------------------------------------
call dein#begin(expand('~/.vim/dein'))

" Plugin mannager
" http://vimawesome.com/plugin/denite-nvim
call dein#add('Shougo/dein.vim')

" Distraction free editing
" http://vimawesome.com/plugin/goyo-vim
call dein#add('junegunn/goyo.vim')

" For writing really fast html
" http://vimawesome.com/plugin/emmet-vim
call dein#add('mattn/emmet-vim')

" Comment all the things!
" http://vimawesome.com/plugin/commentary-vim
call dein#add('tpope/vim-commentary')

" Surrounding/tag editor
" http://vimawesome.com/plugin/surround-vim
call dein#add('tpope/vim-surround')

" Vim linter
" http://vimawesome.com/plugin/syntastic#installation
call dein#add('scrooloose/syntastic')

call dein#end()

" --------------------------------------


" Emmet
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
