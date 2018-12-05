"  ██╗   ██╗██╗███╗   ███╗██████╗  ██████╗
"  ██║   ██║██║████╗ ████║██╔══██╗██╔════╝
"  ██║   ██║██║██╔████╔██║██████╔╝██║
"  ╚██╗ ██╔╝██║██║╚██╔╝██║██╔══██╗██║
"██╗╚████╔╝ ██║██║ ╚═╝ ██║██║  ██║╚██████╗
"╚═╝ ╚═══╝  ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝

" Plugins {{{
    set rtp+=~/.config/nvim/bundle/Vundle.vim
    call vundle#begin('~/.config/nvim/bundle')


" Plugin manager
    Plugin 'VundleVim/Vundle.vim'


" Ctrlp fuzzy finder
    Plugin 'ctrlpvim/ctrlp.vim'
    let g:ctrlp_working_path_mode = 'ra'
    let g:ctrlp_switch_buffer = 'et'
    let g:ctrlp_map = '<leader>p'
    let g:ctrlp_cmd = 'CtrlPMixed'


" Comment things out
    Plugin 'tpope/vim-commentary'


" Display git changes in the gutter
    Plugin 'airblade/vim-gitgutter'


" Easy Motion
    " Plugin 'easymotion/vim-easymotion'
    " let g:EasyMotion_do_mapping = 0
    " let g:EasyMotion_smartcase =  1
    " :nmap <leader>t <Plug>(easymotion-s)


" NertTree file explorer
    Plugin 'scrooloose/nerdtree'

" If NerdTree is the only buffer open close vim
    autocmd bufenter *
        \ if (winnr("$") == 1
        \ && exists("b:NERDTree")
        \ && b:NERDTree.isTabTree())
        \ | q
        \ | endif

" Change the look of the arrows
    let g:NERDTreeDirArrowExpandable = '▸'
    let g:NERDTreeDirArrowCollapsible = '▾'


" Git extension for NerdTree
    Plugin 'Xuyuanp/nerdtree-git-plugin'


" IDE like auto complete
    " Plugin 'valloric/youcompleteme'


" Surround things
    Plugin 'tpope/vim-surround'


" Align code
" TODO: Read through the documentation for this on
" :help tabular
    Plugin 'godlygeek/tabular'


" Use <tab> for completions
    Plugin 'ervandew/supertab'


" Git stuff
    Plugin 'tpope/vim-fugitive'


" Bulk file rename in vim
    Plugin 'qpkorr/vim-renamer'


" A color scheme for vim that uses the default terminal colors
    Plugin 'noahfrederick/vim-noctu'


" Delphi plugin
    Plugin 'rkennedy/vim-delphi'


" Vim org mode
    Plugin 'jceb/vim-orgmode'


" Focus writing mode for vim
    Plugin 'junegunn/goyo.vim'


call vundle#end()
" }}}

" Settings {{{
" Use vim, not vi api
    set nocompatible

    filetype plugin indent on

    set path+=**

" Generates a nice tags file for jummping around
" TODO: See if I can replace this with etags!
" ^] jump to tag
" g^ list ambiguous tags
" ^t jump back up the tag stack
    command! MakeTags !ctags -R --extra=+f .

" Switch syntax highlighting on, when the terminal has colors
    syntax on

" No backup files
    set nobackup
    set nowritebackup

" No swap file
    set noswapfile

" Command history
    set history=100

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
    set tabstop=4

" The number of spaces inserted for a tab (used for auto indenting)
    set shiftwidth=4

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
" set laststatus=2

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

" Ensure Vim doesn't beep at you every time you make a mistype
    set visualbell

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

    autocmd VimLeavePre * call QuitNetrw()

" Always highlight column 80 so it's easier to see where
" cutoff appears on longer screens
    " autocmd BufWinEnter * highlight ColorColumn ctermbg=darkred
    set colorcolumn=80

" Set the colot scheme
    colorscheme noctu
" }}}

" Keybindings {{{
" <leader> is <space>
    let mapleader = " "
    let maplocalleader ='\\'

" Use jk to exit modes (jk not used often in english)
    imap jk <ESC>
    tmap jk <C-\><C-n>

" This one is because I'm lazy. It easy to close windows.
    nmap <leader>q :q <RETURN>

" Quick edit and reload of .vimrc
    nmap <leader>sv :source $MYVIMRC <RETURN>
    nmap <leader>ev :vsplit $MYVIMRC <RETURN>

" More complex keybindings
    nnoremap <leader>i :vsplit <RETURN> :terminal <RETURN> i
    nmap <leader>K :grep! "\b<C-R><C-W>\b"<RETURN>:cw<RETURN>
    nmap <leader>o :setlocal spell! spelllang=en_us<RETURN>
    nmap <leader>T <C-W>T

" Movement and split keybindings
    nmap <leader>\| :vsplit <RETURN>
    nmap <leader>- :split <RETURN>
    nmap <C-J> <C-W><C-J>
    nmap <C-K> <C-W><C-K>
    nmap <C-L> <C-W><C-L>
    nmap <C-H> <C-W><C-H>

" Terminal Keybindings
    tnoremap <Esc> <C-\><C-n>

" Plugin Bindings
    nmap <leader>n :NERDTreeToggle<RETURN>
    nmap <leader>f :Goyo<RETURN>
    nmap <leader>rr :Renamer<RETURN>
" }}}

" Commands {{{
" jump to last cursor
    autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \     exe "normal g`\"" |
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
    autocmd FileType sh,cucumber,ruby,yaml,zsh,delphi setlocal shiftwidth=2 tabstop=2 expandtab

" specify syntax highlighting for specific files
    autocmd Bufread,BufNewFile *.spv set filetype=php
    autocmd Bufread,BufNewFile *.md set filetype=markdown " Vim interprets .md as 'modula2' otherwise, see :set filetype?
    autocmd Bufread,BufNewFile *.pas set filetype=delphi
    autocmd Bufread,BufNewFile *.dpr set filetype=delphi

" Run shell commands and save the results to a buffer
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
        execute '$read !'. expanded_cmdline
        setlocal nomodifiable
        1
    endfunction

" Close all folds when opening a new buffer
    autocmd BufRead * setlocal foldmethod=marker
    autocmd BufRead * normal zM

" Replace grep if ripgrep is installed
    if executable("rg")
        set grepprg=rg\ --vimgrep\ --no-heading\ --color=never
        set grepformat=%f:%l:%c:%m,%f:%l:%m

        " Ctrlp related things
        let g:ctrlp_user_command = 'rg --files %s --color=never --glob ""'
        let g:ctrlp_use_caching = 0
    endif
" }}}
