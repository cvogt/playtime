live-coding-demo-run:
	stack build :live-coding-demo --file-watch --exec live-coding-demo
live-coding-demo-watch:
	stack build :live-coding-demo --exec live-coding-demo
live-coding-demo-compile:
	stack build :live-coding-demo --file-watch
platformer-run:
	stack build :platformer --exec platformer
platformer-watch:
	stack build :platformer --file-watch --exec platformer
platformer-compile:
	stack build :platformer --file-watch
shoot-em-up-run:
	stack build :shoot-em-up  --exec shoot-em-up
shoot-em-up-watch:
	stack build :shoot-em-up --file-watch --exec shoot-em-up
shoot-em-up-compile:
	stack build :shoot-em-up --file-watch
spaceminer-run:
	stack build :spaceminer --exec spaceminer
spaceminer-watch:
	stack build :spaceminer --file-watch --exec spaceminer
spaceminer-compile:
	stack build :spaceminer --file-watch
multispace-run:
	stack build :multispace --exec multispace
multispace-watch:
	stack build :multispace --file-watch --exec multispace
multispace-compile:
	stack build :multispace --file-watch
watch:
	stack test --file-watch --no-run-tests
watch-test:
	stack test --file-watch
format:
	find -L . -type f -name "*.hs" ! -path '*/.stack-work*/*' ! -path '*/dist-newstyle/*' -print0 | \
		\ xargs -0 -n 1 -P 8 (stack build :format --exec "which ormolu") -o -XTypeApplications -o -XBangPatterns -o -XPatternSynonyms -m check
