test:
	stack --no-terminal test --fast --ghc-options="-Werror"

test-watch:
	stack --no-terminal test --fast --ghc-options="-Werror" --file-watch
