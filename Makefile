live-coding-demo-run:
	stack build :live-coding-demo --file-watch --exec live-coding-demo
live-coding-demo-compile:
	stack build :live-coding-demo --file-watch
platformer-run:
	stack build :platformer --file-watch --exec platformer
platformer-compile:
	stack build :platformer --file-watch
shoot-em-up-run:
	stack build :shoot-em-up --file-watch --exec shoot-em-up
shoot-em-up-compile:
	stack build :shoot-em-up --file-watch
spaceminer-run:
	stack build :spaceminer --file-watch --exec spaceminer
spaceminer-compile:
	stack build :spaceminer --file-watch
watch:
	stack build --file-watch
