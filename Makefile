all: trappisto

trappisto: src/*.elm
	elm-make src/Trappisto.elm --yes --warn --output public/assets/js/elm.js
	uglifyjs --mangle --screw-ie8 --output public/assets/js/elm.min.js -- public/assets/js/elm.js
	rm public/assets/js/elm.js

test: src/*.elm tests/*.elm
	elm-test --yes

watch: src/*.elm tests/*.elm
	elm-test --watch

clean:
	$(RM) -r public/assets/js/elm*.js elm-stuff/ tests/elm-stuff/

nginx:
	sudo -u http -- nginx -c nginx/nginx.conf -p . -g 'daemon off;'

cache_buster:
	sudo ./cache_buster.sh

dcrd:
	./dcrd --rpcuser bitcoin --rpcpass secret

.PHONY: clean nginx cache_buster dcrd
