all: static/index.js lib/js/src/compile.js

static/index.js: $(wildcard lib/js/src/*.js)
	node_modules/.bin/browserify -o $@ $^

lib/js/src/index.js lib/js/src/compile.js: src/index.ml
	npm run build
