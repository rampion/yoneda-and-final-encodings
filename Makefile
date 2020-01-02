repl:
	ghcid --command 'cabal repl doctests' --test 'main' --reload=src --restart=final-encodings.cabal

build:
	cabal build

test:
	cabal test

html:
	pandoc --from gfm --standalone > README.html README.md --metadata title="Yoneda and Final Encodings"
