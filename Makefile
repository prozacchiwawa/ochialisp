all: lib/js/src/compile_cmd.js lib/js/src/clvm_cmd.js

lib/js/src/compile_cmd.js lib/js/clvm_cmd.js lib/js/test/test.js: $(wildcard src/*.ml)
	npm run build

test: lib/js/test/test.js
	node ./lib/js/tests/test.js

clean:
	rm -rf lib
