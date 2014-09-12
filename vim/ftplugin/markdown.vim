" Configure vim to use my pandoc-viewer code
function! PandocViewer()
	silent execute "w"
	silent !clear
	silent execute "!pandoc-viewer c " . split(expand('%:t'), ".md")[0]
endfunction
function! PandocCompile()
	silent execute "w"
	silent !clear
	silent execute "!pandoc -V geometry:margin=1in " . expand('%:t') . " -o " . split(expand('%:t'), ".md")[0] . ".pdf"
endfunction
nnoremap <Leader>cc :call PandocCompile()<cr>
