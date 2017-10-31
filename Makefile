all: trappisto

trappisto: src/*.elm
	elm-make src/Trappisto.elm --warn --output public/elm.js
	uglifyjs --mangle --screw-ie8 --output public/elm.min.js -- public/elm.js
	rm public/elm.js

test: src/*.elm tests/*.elm
	elm-test

nginx: nginx/nginx.conf
	nginx -c nginx/nginx.conf -p . -g 'daemon off;'

clean:
	$(RM) public/elm.js
