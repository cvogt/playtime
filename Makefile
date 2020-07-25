watch-run:
	stack build --exec shoot-em-up :shoot-em-up --file-watch --ghc-options -w;tput cnorm
watch:
	stack build --file-watch
