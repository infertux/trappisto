all: trappist0

trappist0: *.elm
	elm-make Main.elm --warn --output elm.js

test: all
	$(MAKE) -C t

nginx:
	nginx -c nginx.conf -p . -g 'daemon off;'

clean:
	$(RM) index.html
