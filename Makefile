all: trappisto

trappisto: src/*.elm
	elm-make src/Trappisto.elm --warn --output public/elm.js

test: all
	elm-test # TODO

nginx: nginx/nginx.conf
	nginx -c nginx/nginx.conf -p . -g 'daemon off;'

clean:
	$(RM) public/elm.js
