all: lib/js/src/compile_cmd.js

lib/js/src/compile_cmd.js: src/compile_cmd.ml
	npm run build

clean:
	rm -rf lib
