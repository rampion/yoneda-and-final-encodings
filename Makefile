ghcid:
	ghcid --command='cabal repl doctests' --test='main' --run=':!make html' --reload=src --restart=final-encodings.cabal

repl:
	cabal repl

build:
	cabal build

test:
	cabal test

html:
	pandoc --from gfm --standalone > README.html README.md --metadata title="Yoneda and Final Encodings"
