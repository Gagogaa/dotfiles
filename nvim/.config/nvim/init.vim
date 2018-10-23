"  ██╗   ██╗██╗███╗   ███╗██████╗  ██████╗
"  ██║   ██║██║████╗ ████║██╔══██╗██╔════╝
"  ██║   ██║██║██╔████╔██║██████╔╝██║
"  ╚██╗ ██╔╝██║██║╚██╔╝██║██╔══██╗██║
"██╗╚████╔╝ ██║██║ ╚═╝ ██║██║  ██║╚██████╗
"╚═╝ ╚═══╝  ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝

" Settings {{{
" Use vim, not vi api
set nocompatible

set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim

filetype plugin on

set path+=**

set wildmenu

" Generates a nice tags file for jummping around
" ^] jump to tag
" g^ list ambiguous tags
" ^t jump back up the tag stack
command! MakeTags !ctags -R --extra=+f .

"if !filereadable(expand('~/.vim/dein/repos/github.com/Shougo/dein.vim.git'))

" !mkdir -p ~/.vim/dein/repos/github.com/Shougo/dein.vim
" !git clone https://github.com/Shougo/dein.vim.git
"   \ ~/.vim/dein/repos/github.com/Shougo/dein.vim

" call dein#begin(expand('~/.vim/dein'))
" call dein#add('Shougo/dein.vim')
" call dein#add('kien/ctrlp.vim')
" call dein#add('easymotion/vim-easymotion')
" call dein#add('Shougo/denite.nvim')
" call dein#add('Shougo/neocomplete.vim')
" call dein#end()
" call dein#install()
"endif

" Switch syntax highlighting on, when the terminal has colors
syntax on

" No backup files
set nobackup

" No write backup
set nowritebackup

" No swap file
" set noswapfile

" Command history
set history=100

" Always show cursor
" TODO: find out what this does
" set ruler

" Show incomplete commands
set showcmd

" Incremental searching (search as you type)
set incsearch

" Highlight search matches
set hlsearch

" Ignore case in search
set smartcase

" Make sure any searches /searchPhrase doesn't need the \c escape character
set ignorecase

" A buffer is marked as ‘hidden’ if it has unsaved changes, and it is not currently loaded in a window
" if you try and quit Vim while there are hidden buffers, you will raise an error:
" E162: No write since last change for buffer “a.txt”
set hidden

" Turn word wrap off
set nowrap

" Allow backspace to delete end of line, indent and start of line characters
set backspace=indent,eol,start

" Convert tabs to spaces
set expandtab

" Set tab size in spaces (this is for manual indenting)
set tabstop=2

" The number of spaces inserted for a tab (used for auto indenting)
set shiftwidth=2

" Turn on line numbers
set number

" Turn on all mouse functions
set mouse=a

" Keep some space above and below the cursor at all times
set scrolloff=5

" Highlight tailing whitespace
" See issue: https://github.com/Integralist/ProVim/issues/4
set list listchars=tab:\ \ ,trail:·

" Get rid of the delay when pressing O (for example)
" http://stackoverflow.com/questions/2158516/vim-delay-before-o-opens-a-new-line
set timeout timeoutlen=1000 ttimeoutlen=100

" Always show status bar
set laststatus=2

" Set the status line to something useful
set statusline=%f\ %=L:%l/%L\ %c\ (%p%%)

" Hide the toolbar
set guioptions-=T

" UTF encoding
set encoding=utf-8

" Autoload files that have changed outside of vim
set autoread

" Use system clipboard
" http://stackoverflow.com/questions/8134647/copy-and-paste-in-vim-via-keyboard-between-different-mac-terminals
set clipboard+=unnamed

" Don't show intro
set shortmess+=I

" Better splits (new windows appear below and to the right)
set splitbelow
set splitright

" Highlight the current line
" set cursorline

" Ensure Vim doesn't beep at you every time you make a mistype
"set visualbell

" Visual autocomplete for command menu (e.g. :e ~/path/to/file)
set wildmenu

" redraw only when we need to (i.e. don't redraw when executing a macro)
set lazyredraw

" highlight a matching [{()}] when cursor is placed on start/end character
set showmatch

" Use tabs instead of splits when possible
set switchbuf=usetab,newtab

" Set built-in file system explorer to use layout similar to the NERDTree plugin
let g:netrw_liststyle=3

" Kill netrw buffers because they will not let you close vim even with q!
function! QuitNetrw()
  for i in range(1, bufnr($))
    if buflisted(i)
      if getbufvar(i, '&filetype') == "netrw"
        silent exe 'bwipeout ' . i
      endif
    endif
  endfor
endfunction

autocmd VimLeavePre *  call QuitNetrw()

" Always highlight column 80 so it's easier to see where
" cutoff appears on longer screens
"autocmd BufWinEnter * highlight ColorColumn ctermbg=darkred
"set colorcolumn=80
" }}}

" Keybindings {{{
" <leader> is <space>
:let mapleader = " "
:let maplocalleader ='\\'

:inoremap jk <ESC>

" Auto insert matching charaters
:ino " ""<left>
:ino ' ''<left>
:ino ( ()<left>
:ino [ []<left>
:ino { {}<left>
:ino {<CR> {<CR>}<ESC>O
:ino {;<CR> {<CR>};<ESC>O

:nnoremap <leader>sv :source $MYVIMRC <RETURN>
:nnoremap <leader>ev :vsplit $MYVIMRC <RETURN>
:nnoremap <leader>\|  :vsplit <RETURN>
:nnoremap <leader>-  :split <RETURN>
:nnoremap <F1> @d
:nnoremap <F2> qd
:nnoremap <leader>w :w <RETURN>
:nnoremap <leader>q :q <RETURN>
:nnoremap <leader>Q :q! <RETURN>

:nnoremap <leader>p :CtrlP<RETURN>
:nnoremap <leader>b :CtrlPBuffer<RETURN>
:nnoremap <C-P> :CtrlP<RETURN>
:nnoremap <leader>di :call dein#install()<RETURN>
:nnoremap <leader>t <Plug>(easymotion-prefix)

:nnoremap <leader>a 0
:nnoremap <leader>f $


:nnoremap <leader>j <C-W><C-J>
:nnoremap <leader>k <C-W><C-K>
:nnoremap <leader>l <C-W><C-L>
:nnoremap <leader>h <C-W><C-H>

:nnoremap <C-J> <C-W><C-J>
:nnoremap <C-K> <C-W><C-K>
:nnoremap <C-L> <C-W><C-L>
:nnoremap <C-H> <C-W><C-H>

" Terminal mode map
:tnoremap <Esc> <C-\><C-n>
:tnoremap jk <C-\><C-n>
" }}}

" I have no idea what these do so I left them here after using the provim config
" Commands {{{
" jump to last cursor
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

fun! StripTrailingWhitespace()
  " don't strip on these filetypes
  if &ft =~ 'markdown'
    return
  endif
  %s/\s\+$//e
endfun
autocmd BufWritePre * call StripTrailingWhitespace()

" file formats
autocmd Filetype gitcommit setlocal spell textwidth=72
autocmd Filetype markdown setlocal wrap linebreak nolist textwidth=0 wrapmargin=0 " http://vim.wikia.com/wiki/Word_wrap_without_line_breaks
autocmd FileType sh,cucumber,ruby,yaml,zsh,vim setlocal shiftwidth=2 tabstop=2 expandtab

" specify syntax highlighting for specific files
autocmd Bufread,BufNewFile *.spv set filetype=php
autocmd Bufread,BufNewFile *.md set filetype=markdown " Vim interprets .md as 'modula2' otherwise, see :set filetype?
autocmd Bufread,BufNewFile *.pas set filetype=delphi
autocmd Bufread,BufNewFile *.dpr set filetype=delphi

" Highlight words to avoid in tech writing
" http://css-tricks.com/words-avoid-educational-writing/
highlight TechWordsToAvoid ctermbg=red ctermfg=white
match TechWordsToAvoid /\cobviously\|basically\|simply\|of\scourse\|clearly\|just\|everyone\sknows\|however\|so,\|easy/
autocmd BufWinEnter * match TechWordsToAvoid /\cobviously\|basically\|simply\|of\scourse\|clearly\|just\|everyone\sknows\|however,\|so,\|easy/
autocmd InsertEnter * match TechWordsToAvoid /\cobviously\|basically\|simply\|of\scourse\|clearly\|just\|everyone\sknows\|however,\|so,\|easy/
autocmd InsertLeave * match TechWordsToAvoid /\cobviously\|basically\|simply\|of\scourse\|clearly\|just\|everyone\sknows\|however,\|so,\|easy/
autocmd BufWinLeave * call clearmatches()

" Create a 'scratch buffer' which is a temporary buffer Vim wont ask to save
" http://vim.wikia.com/wiki/Display_output_of_shell_commands_in_new_window
command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
function! s:RunShellCommand(cmdline)
  echo a:cmdline
  let expanded_cmdline = a:cmdline
  for part in split(a:cmdline, ' ')
    if part[0] =~ '\v[%#<]'
      let expanded_part = fnameescape(expand(part))
      let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
    endif
  endfor
  botright new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  call setline(1, 'You entered:    ' . a:cmdline)
  call setline(2, 'Expanded Form:  ' .expanded_cmdline)
  call setline(3,substitute(getline(2),'.','=','g'))
  execute '$read !'. expanded_cmdline
  setlocal nomodifiable
  1
endfunction

" Close all folds when opening a new buffer
autocmd BufRead * setlocal foldmethod=marker
autocmd BufRead * normal zM

" Rainbow parenthesis always on!
"if exists(':RainbowParenthesesToggle')
"  autocmd VimEnter * RainbowParenthesesToggle
"  autocmd Syntax * RainbowParenthesesLoadRound
"  autocmd Syntax * RainbowParenthesesLoadSquare
"  autocmd Syntax * RainbowParenthesesLoadBraces
"endif

" Reset spelling colours when reading a new buffer
" This works around an issue where the colorscheme is changed by .local.vimrc
fun! SetSpellingColors()
  highlight SpellBad cterm=bold ctermfg=white ctermbg=red
  highlight SpellCap cterm=bold ctermfg=red ctermbg=white
endfun
autocmd BufWinEnter * call SetSpellingColors()
autocmd BufNewFile * call SetSpellingColors()
autocmd BufRead * call SetSpellingColors()
autocmd InsertEnter * call SetSpellingColors()
autocmd InsertLeave * call SetSpellingColors()

" Change colourscheme when diffing
fun! SetDiffColors()
  highlight DiffAdd    cterm=bold ctermfg=white ctermbg=DarkGreen
  highlight DiffDelete cterm=bold ctermfg=white ctermbg=DarkGrey
  highlight DiffChange cterm=bold ctermfg=white ctermbg=DarkBlue
  highlight DiffText   cterm=bold ctermfg=white ctermbg=DarkRed
endfun
autocmd FilterWritePre * call SetDiffColors()

" }}}
