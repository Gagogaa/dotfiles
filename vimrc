"Basic setup 
":echo ">^.^<"
:set shiftwidth=2
:set autoindent
:set tabstop=2
:set number
:set relativenumber
:set numberwidth=4
:set nowrap
":set colorcolumn=80
let localleader=","
let mapleader ="\<Space>"

"Remove annoying bars
:set guioptions-=m
:set guioptions-=T
:set guioptions-=r
:set guioptions-=L

"Normal mappings
nnoremap <leader>u :set spell spelllang=en_us<cr>
nnoremap <leader><leader>u :set nospell spelllang=en_us<cr>
nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader><leader>q :q!<cr>
nnoremap <leader>x :x<cr>
nnoremap <leader>z ZZ<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
"Makes H and L move to start and end of line respectivly
nnoremap H 0
nnoremap L $

"Insert mappings
inoremap <c-d> <esc>ddi
"inoremap jk <esc>

"Visual mappings
vnoremap " d<esc>i""<esc>hpl

"Auto commands
:autocmd BufNewFile *.* :write
":autocmd FileType vim nnoremap <buffer> <localleader>c I"<esc>
 
