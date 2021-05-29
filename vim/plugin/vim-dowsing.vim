function! Dowse (args)
	let args = join (mapnew (split (a:args, ','), 'shellescape (trim (v:val))'))
	execute 'botright vnew'
	execute 'setlocal buftype=nofile bufhidden=hide noswapfile'
	execute '0read !dowsindex search ' . args
	execute 'normal! gg'
	execute 'setlocal filetype=ocaml readonly'
endfunction

autocmd FileType ocaml command! -buffer -nargs=* Dowse call Dowse (<q-args>)
