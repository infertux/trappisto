all: trappisto

trappisto: src/*.elm
	elm-make src/Trappisto.elm --warn --output public/assets/js/elm.js
	uglifyjs --mangle --screw-ie8 --output public/assets/js/elm.min.js -- public/assets/js/elm.js
	rm public/assets/js/elm.js

test: src/*.elm tests/*.elm
	elm-test

nginx: nginx/nginx.conf
	nginx -c nginx/nginx.conf -p . -g 'daemon off;'

clean:
	$(RM) public/assets/js/elm*.js
