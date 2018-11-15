" Setup Vundle
filetype off
set nocompatible
set shell=bash
call plug#begin('~/.vim/plugged')

" Search, File Directories...
Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/fzf.vim' " Fuzzy finder search
Plug 'scrooloose/nerdtree' " File navigation tree
Plug 'jistr/vim-nerdtree-tabs'
Plug 'tpope/vim-unimpaired'

" Buffer plugins
Plug 'rbgrouleff/bclose.vim' " Close buffers without closing window
Plug 'vim-scripts/bufkill.vim'
Plug 'jlanzarotta/bufexplorer' " Better buffer explorer
Plug 'ap/vim-buftabline'

" Language Support
Plug 'w0rp/ale'
Plug 'pangloss/vim-javascript', {'for': 'js'}
Plug 'vim-pandoc/vim-pandoc', {'for': 'md'}
Plug 'vim-pandoc/vim-pandoc-syntax', {'for': 'md'}
Plug 'lervag/vimtex', {'for': 'tex'}
Plug 'vim-scripts/nginx.vim'
Plug 'ekalinin/Dockerfile.vim'
Plug 'elzr/vim-json', {'for': 'json'}
Plug 'vim-scripts/SQLComplete.vim', {'for': 'sql'}
Plug 'rust-lang/rust.vim', {'for': 'rs'}
Plug 'racer-rust/vim-racer', {'for': 'rs'}
Plug 'vim-scripts/indentpython.vim'
Plug 'dag/vim-fish', {'for': 'fish'}

" Stylistic
Plug 'jacoborus/tender.vim'
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-startify'

" Auto completion and snippets
Plug 'szw/vim-tags' " ctags support
Plug 'Valloric/YouCompleteMe' " Tab autocompletion
Plug 'davidhalter/jedi-vim' " Python jedi support

" Utility
Plug 'scrooloose/nerdcommenter' " Comment code easily
Plug 'jiangmiao/auto-pairs' " Auto add pairing delimiters
Plug 'tmhedberg/SimpylFold' " Code folding
Plug 'easymotion/vim-easymotion'
call plug#end()

syntax on
filetype plugin indent on
set encoding=utf-8
set showcmd
set foldlevel=3
set term=xterm-256color

" Configure line number stuff
set number
set cursorline
set scrolloff=3
let g:UseNumberToggleTrigger = 0

" Write out old file while switching contexts
set autowrite
set ruler

" Remap leader
let mapleader=","

" Enable code folding
set foldenable

" Easier vertical split
nnoremap <leader>v <C-w>v<C-w>l

" Default tabstop and shiftwidth
set tabstop=2 shiftwidth=2
set backspace=indent,eol,start
set expandtab

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
set guiheadroom=0

" Enable mouse
set mouse=a

" Set my color scheme and preferred font
set guifont=Fira\ Code\ 11
set guioptions-=m
set guioptions-=T
set guioptions-=r

colorscheme tender

"
let NERDTreeIgnore = ['\.pyc$']

" YCM Config
map <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
" Remap autocomplete to something more natural
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>

" GitGutter styling to use · instead of +/-
let g:gitgutter_sign_added = '∙'
let g:gitgutter_sign_modified = '∙'
let g:gitgutter_sign_removed = '∙'
let g:gitgutter_sign_modified_removed = '∙'

let g:ale_sign_warning = '▲'
let g:ale_sign_error = '✗'

let g:jedi#rename_command = ""

" Set preview/scratch off
set completeopt=menu

" Let me have command autocomplete like in bash
set wildmenu
set wildmode=list:longest

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

" Hotkey for closing a buffer
nnoremap <Leader>3 :bnext<cr>
nnoremap <Leader>2 :Bclose<cr>
nnoremap <Leader>1 :bprevious<cr>


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

" Set spell check for markdown
autocmd BufRead,BufNewFile *.md setlocal spell
autocmd FileType gitcommit setlocal spell

" Configure LaTeX-Box
let g:LatexBox_latexmk_preview_continuously = 1
let g:LatexBox_quickfix = 4

" Configure LaTeX-Box
let g:LatexBox_latexmk_options = "-pvc -pdfps"

" Auto open nerdtree and close when its the only thing left
" autocmd vimenter * NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
nnoremap <C-N> :NERDTreeToggle<CR>

" Start vim with file focused instead of nerdtree
autocmd VimEnter * wincmd p
autocmd VimEnter * if (line('$') == 1 && getline(1) == '') | wincmd p | endif

" Disable YCM for C/C++ for Grappa project
let g:ycm_filetype_blacklist = {
      \ 'tagbar' : 1,
      \ 'qf' : 1,
      \ 'notes' : 1,
      \ 'markdown' : 1,
      \ 'unite' : 1,
      \ 'text' : 1,
      \ 'vimwiki' : 1,
      \ 'pandoc' : 1,
      \ 'infolog' : 1,
      \ 'mail' : 1}

" Save file when focus is lost
autocmd BufLeave,FocusLost * wall

" Eliminate delay from insert to normal mode
set timeoutlen=1000 ttimeoutlen=0

let g:vim_json_syntax_conceal = 0

" Change cursor modes
au InsertEnter * silent execute "!echo -en \<esc>[5 q"
au InsertLeave * silent execute "!echo -en \<esc>[2 q"

nmap ' :Buffers<CR>
nmap <Leader>f :Files<CR>
nmap <Leader>t :Tags<CR>

let g:lightline = {
      \ 'colorscheme': 'wombat',
      \ 'active': {
      \   'left': [['mode', 'paste'], ['filename', 'modified']],
      \   'right': [['lineinfo'], ['percent'], ['readonly', 'linter_warnings', 'linter_errors', 'linter_ok']]
      \ },
      \ 'component_expand': {
      \   'linter_warnings': 'LightlineLinterWarnings',
      \   'linter_errors': 'LightlineLinterErrors',
      \   'linter_ok': 'LightlineLinterOK'
      \ },
      \ 'component_type': {
      \   'linter_warnings': 'warning',
      \   'linter_errors': 'error'
      \ }
\ }


let g:ale_python_pylint_options = '--max-line-length=120'

function! LightlineLinterWarnings() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '' : printf('%d ◆', all_non_errors)
endfunction

function! LightlineLinterErrors() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '' : printf('%d ✗', all_errors)
endfunction

function! LightlineLinterOK() abort
  let l:counts = ale#statusline#Count(bufnr(''))
  let l:all_errors = l:counts.error + l:counts.style_error
  let l:all_non_errors = l:counts.total - l:all_errors
  return l:counts.total == 0 ? '✓ ' : ''
endfunction

autocmd User ALELint call s:MaybeUpdateLightline()

" Update and show lightline but only if it's visible (e.g., not in Goyo)
function! s:MaybeUpdateLightline()
  if exists('#lightline')
    call lightline#update()
  end
endfunction

autocmd FileType latex setlocal spell
autocmd BufRead,BufNewFile *.tex setlocal spell
