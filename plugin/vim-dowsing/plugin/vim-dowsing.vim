function! Dowse (query)
	execute "botright vnew"
	execute "setlocal buftype=nofile bufhidden=hide noswapfile"
	execute "0read !dowsindex search " . shellescape (a:query)
	execute "normal! gg"
	execute "setlocal filetype=ocaml readonly"
endfunction

autocmd FileType ocaml command! -buffer -nargs=1 Dowse call Dowse (<args>)
