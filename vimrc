" Kick off some plugins
execute pathogen#infect()
syntax on
filetype plugin indent on
set nocompatible
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
set guifont=Anonymous\ Pro:h12
colorscheme molokai

" Remap autocomplete to something more natural
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>

" Let me have command autocomplete like in bash
set wildmenu
set wildmode=list:longest

" Auto open nerdtree and close when its the only thing left
" autocmd vimenter * NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
map <C-n> :NERDTreeToggle<CR>
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

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
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'

" Enable ctrl-tab and ctrl-shift-tab to switch between buffers
map <C-Tab> :bnext<cr>
map <C-S-Tab> :bprevious<cr>
map <F5> :bnext<cr>
map :bc :Bclose
let g:colorpicker_app = 'iTerm.app'

augroup nonvim
   au!
   au BufRead *.png,*.jpg,*.pdf,*.gif,*.xls* sil exe "!open " . shellescape(expand("%:p")) | bd | let &ft=&ft
   au BufRead *.ppt*,*.doc*,*.rtf let g:output_pdf = shellescape(expand("%:r") . ".pdf")
   au BufRead *.ppt*,*.doc*,*.rtf sil exe "!/usr/local/bin/any2pdf " . shellescape(expand("%:p"))
   au BufRead *.ppt*,*.doc*,*.rtf sil exe "!open " . g:output_pdf | bd | let &ft=&ft
augroup end

" Force *.md files to be recognized as markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
" Fix problem with markdown browser viewer
set shell=bash\ -i

" Disable pandoc changing symbols in markdown
let g:pandoc#syntax#conceal#use = 0
