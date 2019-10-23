"   ██╗   ██╗██╗███╗   ███╗██████╗  ██████╗
"   ██║   ██║██║████╗ ████║██╔══██╗██╔════╝
"   ██║   ██║██║██╔████╔██║██████╔╝██║
"   ╚██╗ ██╔╝██║██║╚██╔╝██║██╔══██╗██║
" ██╗╚████╔╝ ██║██║ ╚═╝ ██║██║  ██║╚██████╗
" ╚═╝ ╚═══╝  ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝

" Plugins {{{
    set rtp+=~/.config/nvim/bundle/Vundle.vim
    call vundle#begin('~/.config/nvim/bundle')


" Plugin manager
    Plugin 'VundleVim/Vundle.vim'


" Ctrlp fuzzy finder
    Plugin 'ctrlpvim/ctrlp.vim'
    let g:ctrlp_working_path_mode = 'ra'
    let g:ctrlp_switch_buffer = 'et'
    let g:ctrlp_root_markers = ['requirements.txt']
    let g:ctrlp_map = '<leader>p'
    let g:ctrlp_cmd = 'CtrlP'


" Comment things out
    Plugin 'tpope/vim-commentary'


" Display git changes in the gutter
" TODO See if I can edit the colors / symbols that are used
" Also read the documentation on this because it seems to have way more
" features than just displaying gutters.
     Plugin 'airblade/vim-gitgutter'


" Easy Motion
    Plugin 'easymotion/vim-easymotion'
    let g:EasyMotion_do_mapping = 0
    let g:EasyMotion_smartcase =  1
    :nmap <leader>t <Plug>(easymotion-s)


" IDE like auto complete
    " Plugin 'valloric/youcompleteme'


" Surround things
    Plugin 'tpope/vim-surround'


" Align code
" TODO: Read through the documentation for this on
" :help tabular
    Plugin 'godlygeek/tabular'


" Use <tab> for completions
" NOTE: <c-n> in insert mode works too
    " Plugin 'ervandew/supertab'


" Git stuff
    Plugin 'tpope/vim-fugitive'


" Bulk file rename in vim
    Plugin 'qpkorr/vim-renamer'


" A color scheme for vim that uses the default terminal colors
    Plugin 'noahfrederick/vim-noctu'


" Delphi plugin
    Plugin 'rkennedy/vim-delphi'


" Focus writing mode for vim
    Plugin 'junegunn/goyo.vim'


" Meson syntax and more
    Plugin 'igankevich/mesonic'


" Quick highlight the line the cursor is on when searching in vim
    Plugin 'inside/vim-search-pulse'
    let g:vim_search_pulse_duration = 200


" Much better python syntax highlighting
    " Plugin 'numirias/semshi'


" Better markdown plugin
    Plugin 'plasticboy/vim-markdown'


" Add scratch buffers to vim
    Plugin 'vim-scripts/scratch.vim'


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

" A buffer is marked as ‘hidden’ if it has unsaved changes, and it is not
" currently loaded in a window if you try and quit Vim while there are hidden
" buffers, you will raise an error:
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
    set number relativenumber

" Turn on all mouse functions
    set mouse=a

" Keep some space above and below the cursor at all times
    set scrolloff=5

" Highlight tailing whitespace
" See issue: https://github.com/Integralist/ProVim/issues/4
    set list listchars=tab:>-,trail:·

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
    set clipboard+=unnamedplus

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
    let g:netrw_liststyle=3 " Use the Tree style layout
    let g:netrw_banner=0 " Remove the banner
    " let g:netrw_browse_split = 1 " Better split keybindings

" Kill netrw buffers because they will not let you close vim even with q!
" TODO this function does not work
    " function! QuitNetrw()
    "     for i in range(1, bufnr($))
    "         if buflisted(i)
    "             if getbufvar(i, '&filetype') == "netrw"
    "                 silent exe 'bwipeout ' . i
    "             endif
    "         endif
    "     endfor
    " endfunction

    " autocmd VimLeavePre * call QuitNetrw()

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

" Quick edit and reload .vimrc
    nmap <leader>sv :source $MYVIMRC <RETURN>
    nmap <leader>ev :vsplit $MYVIMRC <RETURN>

" More complex keybindings
    " Open a terminal
    nnoremap <leader>i :vsplit <RETURN> :terminal <RETURN> i
    " Search for word under cursor in current project
    nmap <leader>K :grep! "\b<C-R><C-W>\b"<RETURN>:cw<RETURN><RETURN>
    " Set spelling in the current buffer
    nmap <leader>o :setlocal spell! spelllang=en_us<RETURN>
    " Move the current buffer to a new tab
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
    " nmap <leader>n :NERDTreeToggle<RETURN>
    nmap <leader>f :Goyo<RETURN>
    nmap <leader>rr :Renamer<RETURN>
    nmap <leader>b :CtrlPBuffer<RETURN>
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
    " http://vim.wikia.com/wiki/Word_wrap_without_line_breaks
    autocmd Filetype markdown setlocal wrap linebreak nolist textwidth=0 wrapmargin=0
    autocmd FileType sh,yaml,zsh,delphi,md,html setlocal shiftwidth=2 tabstop=2 expandtab
    autocmd FileType delphi setlocal foldmethod=indent


" specify syntax highlighting for specific files
    autocmd Bufread,BufNewFile *.md set filetype=markdown " Vim interprets .md as 'modula2' otherwise, see :set filetype?
    autocmd Bufread,BufNewFile *.pas set filetype=delphi  " Use the new delphi syntax instead of the old pascal syntax
    autocmd Bufread,BufNewFile *.dpr set filetype=delphi


" Run shell commands and save the results to a buffer
" http://vim.wikia.com/wiki/Display_output_of_shell_commands_in_new_window
" TODO remove this hack of a function for one that resembles the commands at
" the bottom of the wiki
    command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
    function! s:RunShellCommand(cmdline)
        let isfirst = 1
        let words = []
        for word in split(a:cmdline)
            if isfirst
                let isfirst = 0  " don't change first word (shell command)
            else
                if word[0] =~ '\v[%#<]'
                    let word = expand(word)
                endif
                let word = shellescape(word, 1)
            endif
            call add(words, word)
        endfor
        let expanded_cmdline = join(words)
        botright new
        setlocal buftype=nowrite bufhidden=wipe nobuflisted noswapfile nowrap
        silent execute '$read !'. expanded_cmdline
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


" Visually select a block of code and press ~ to cycle between cases
    function! TwiddleCase(str)
        if a:str ==# toupper(a:str)
            let result = tolower(a:str)
        elseif a:str ==# tolower(a:str)
            let result = substitute(a:str,'\(\<\w\+\>\)', '\u\1', 'g')
        else
            let result = toupper(a:str)
        endif
        return result
    endfunction
    vnoremap ~ y:call setreg('', TwiddleCase(@"), getregtype(''))<CR>gv""Pgv


" Add todo highlighting
    augroup myTodo
        autocmd!
        autocmd Syntax *  syntax match myTodo /\v\_.<(TODO|FIXME|NOTE|FIX),*/hs=s+1 containedin=.*Comment
    augroup END
    highlight link myTodo Todo

" Highlight
" }}}
