all: trappist0

trappist0: *.elm
	elm-make --warn Main.elm

test: all
	$(MAKE) -C t

nginx:
	nginx -c nginx.conf -p . -g 'daemon off;'

clean:
	$(RM) index.html
