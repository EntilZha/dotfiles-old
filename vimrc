" Setup Vundle
filetype off
set nocompatible
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Framework Vim
Plugin 'gmarik/Vundle.vim'

" Search, File Directories...
Plugin 'mileszs/ack.vim' " Access to Ack search
Plugin 'kien/ctrlp.vim' " Fuzzy finder search
Plugin 'scrooloose/nerdtree' " File navigation tree

" Buffer plugins
Plugin 'rbgrouleff/bclose.vim' " Close buffers without closing window
Plugin 'bufkill.vim'
Plugin 'jlanzarotta/bufexplorer' " Better buffer explorer

" Language Support
Plugin 'scrooloose/syntastic' " Syntax for code
Plugin 'burnettk/vim-angular' " Angular support
Plugin 'tpope/vim-bundler' " Bundler support
Plugin 'tpope/vim-fugitive' " Git support
Plugin 'fatih/vim-go' " Golang Support
Plugin 'nsf/gocode', {'rtp': 'vim/'}
Plugin 'pangloss/vim-javascript' " Javascript support
Plugin 'vim-pandoc/vim-pandoc' " Pandoc support
Plugin 'vim-pandoc/vim-pandoc-syntax' " Pandoc syntax support
Plugin 'tpope/vim-rails' " Rails support
Plugin 'vim-ruby/vim-ruby' " Ruby support
Plugin 'mustache/vim-mustache-handlebars' " Handlebars support
Plugin 'LaTeX-Box-Team/LaTeX-Box' " LaTeX support
Plugin 'derekwyatt/vim-scala' " Scala support
Plugin 'vim-scripts/nginx.vim' "Nginx support
Plugin 'ekalinin/Dockerfile.vim' "Dockerfile support
Plugin 'elzr/vim-json' "Json support

" Stylistic
Plugin 'Lokaltog/powerline-fonts' " Fonts for powerline/airline
Plugin 'bling/vim-airline' " Nice status bar
Plugin 'airblade/vim-gitgutter' " Git visual support

" Auto completion and snippets
Plugin 'szw/vim-tags' " ctags support
Plugin 'SirVer/ultisnips' " Snippets
Plugin 'honza/vim-snippets' " Starter snippets
Plugin 'Valloric/YouCompleteMe' " Tab autocompletion

" Utility
Plugin 'scrooloose/nerdcommenter' " Comment code easily
Plugin 'Raimondi/delimitMate' " Auto add pairing delimiters
Plugin 'rizzatti/dash.vim' " Dash support
Plugin 'jeffkreeftmeijer/vim-numbertoggle' " Switch line numbering in cmd vs insert mode

call vundle#end()

syntax on
filetype plugin indent on
set encoding=utf-8
set showcmd

" Configure line number stuff
set number
set relativenumber
set cursorline
set scrolloff=3

" Write out old file while switching contexts
set autowrite
set ruler

" Remap leader
let mapleader=","

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

" Disable backup and swapfile
set noswapfile
set nobackup

" Set gui options
set guioptions-=r
set guioptions-=L

" Set my color scheme and preferred font
set guifont=Anonymous\ Pro\ for\ Powerline:h12
colorscheme molokai

" Remap autocomplete to something more natural
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>

" Set preview/scratch off
set completeopt=menu

" Let me have command autocomplete like in bash
set wildmenu
set wildmode=list:longest

" Write as sudo when needed
cmap w!! w !sudo tee > /dev/null %

" Auto open nerdtree and close when its the only thing left
" autocmd vimenter * NERDTree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
nnoremap <C-N> :NERDTreeToggle<CR>

" Start vim with file focused instead of nerdtree
autocmd VimEnter * wincmd p
autocmd VimEnter * if (line('$') == 1 && getline(1) == '') | wincmd p | endif

" Remap pane switching shortcuts
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Treat long lines as break lines
map j gj
map k gk

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
nnoremap <C-Tab> :bnext<cr>
nnoremap <C-S-Tab> :bprevious<cr>

" Hotkey for closing a buffer
nnoremap <Leader>bc :Bclose<cr>


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

" Configure LaTeX-Box
let g:LatexBox_latexmk_preview_continuously = 1
let g:LatexBox_quickfix = 4

" Unhighlight
nnoremap <Leader>hh :nohl<cr>
nnoremap <Leader>hn :hl<cr>

" Configure Ultisnips to play nice with YCM
let g:UltiSnipsExpandTrigger = '<C-s>'
let g:UltiSnipsJumpForwardTrigger = '<Tab>'
let g:UltiSnipsJumpBackwardTrigger = '<S-Tab>'
"let g:UltiSnipsListSnippets = '<c-m>'

" Configure LaTeX-Box
let g:LatexBox_latexmk_options = "-pvc -pdfps"

" vim-go set auto importer
let g:go_fmt_command = "goimports"

let g:NumberToggleTrigger = "<F2>"
