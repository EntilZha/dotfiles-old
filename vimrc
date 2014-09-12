" Setup Vundle
filetype off
set nocompatible
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'mileszs/ack.vim'
Plugin 'rbgrouleff/bclose.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/syntastic'
Plugin 'Lokaltog/powerline-fonts'
" Plugin 'Lokaltog/powerline'
Plugin 'bling/vim-airline'
Plugin 'burnettk/vim-angular'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-fugitive'
Plugin 'fatih/vim-go'
Plugin 'pangloss/vim-javascript'
Plugin 'vim-pandoc/vim-pandoc'
Plugin 'vim-pandoc/vim-pandoc-syntax'
Plugin 'tpope/vim-rails'
Plugin 'vim-ruby/vim-ruby'
Plugin 'derekwyatt/vim-scala'
Plugin 'szw/vim-tags'
Plugin 'mustache/vim-mustache-handlebars'

" Vimscripts
Plugin 'bufkill.vim'

call vundle#end()

syntax on
filetype plugin indent on
set encoding=utf-8
set showcmd

" Show line numbers by default
set number

" Write out old file while switching contexts
set autowrite
set ruler
set nowrap

" Enable code folding
set foldenable
nnoremap <leader>ft Vatzf

" Easier vertical split
nnoremap <leader>v <C-w>v<C-w>l

" Default tabstop and shiftwidth
set tabstop=2 shiftwidth=2
set backspace=indent,eol,start

" Configure search
set hlsearch
set incsearch
set ignorecase
set smartcase

" Set my color scheme and preferred font
set guifont=Anonymous\ Pro\ for\ Powerline:h12
colorscheme molokai

" Remap autocomplete to something more natural
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>

" Let me have command autocomplete like in bash
set wildmenu
set wildmode=list:longest

" Auto open nerdtree and close when its the only thing left
" autocmd vimenter * NERDTree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
nnoremap <C-n> :NERDTreeToggle<CR>

" Start vim with file focused instead of nerdtree
autocmd VimEnter * wincmd p
autocmd VimEnter * if (line('$') == 1 && getline(1) == '') | wincmd p | endif

" Remap pane switching shortcuts
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Set splits to be natural
set splitbelow
set splitright

" Make the preview of methods on autocomplete close once a selection is made
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" Enable list of buffers and show only filename
" python from powerline.vim import setup as powerline_setup
" python powerline_setup()
" python del powerline_setup
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
set laststatus=2
set noshowmode

" Enable short cuts for switching buffers
nnoremap <Leader>bn :bnext<cr>
nnoremap <Leader>bp :bprevious<cr>

" Hotkey for closing a buffer
nnoremap <Leader>w :Bclose<cr>

" Open files that are not vim in their correct program
augroup nonvim
   au!
   au BufRead *.png,*.jpg,*.pdf,*.gif,*.xls* sil exe "!open " . shellescape(expand("%:p")) | bd | let &ft=&ft
   au BufRead *.ppt*,*.doc*,*.rtf let g:output_pdf = shellescape(expand("%:r") . ".pdf")
   au BufRead *.ppt*,*.doc*,*.rtf sil exe "!/usr/local/bin/any2pdf " . shellescape(expand("%:p"))
   au BufRead *.ppt*,*.doc*,*.rtf sil exe "!open " . g:output_pdf | bd | let &ft=&ft
augroup end

" Force *.md files to be recognized as markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Disable pandoc changing symbols in markdown
let g:pandoc#syntax#conceal#use = 0

" Don't let vim make a bell on entering
set noerrorbells
set novisualbell
autocmd! GUIEnter * set vb t_vb=

" Highlight whitespaces at end of line not on current editing line
autocmd InsertEnter * syn clear EOLWS | syn match EOLWS excludenl /\s\+\%#\@!$/
autocmd InsertLeave * syn clear EOLWS | syn match EOLWS excludenl /\s\+$/
highlight EOLWS ctermbg=blue guibg=#AAD7E6

" Configure Pandoc to not fold so much
let g:pandoc#folding#level = 4

" Convention for me is to map <Leader>c to whatever the related
" compile function is in the langauge. This occurs in the ftplugin

" Make it easy to open my vimrc
nnoremap <Leader>ev :vsplit $MYVIMRC<cr>
nnoremap <Leader>sv :source $MYVIMRC<cr>

" Set spell check for markdown
autocmd BufRead,BufNewFile *.md setlocal spell
autocmd FileType gitcommit setlocal spell
