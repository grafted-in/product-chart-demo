all: out

app:
	cabal build

out: app
	mkdir -p out
	cp dist/build/main/main.jsexe/* out
	cp -r static/ out

prod: out
	closure-compiler -O ADVANCED out/all.js > out/all.min.js
	mv out/static/index.html.prod out/index.html


clean:
	rm -rf out
