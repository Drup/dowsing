install:
	vundle:
		add to vundle plugins: "Plugin 'Drup/dowsing', { 'rtp' : 'vim' }"

examples:
	:Dowse int -> int
	:Dowse stdlib, int -> int
	:Dowse stdlib, containers, int -> int
