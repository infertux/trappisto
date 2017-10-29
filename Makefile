all: trappist0

trappist0: Main.elm
	elm-make --warn $^

test: all
	$(MAKE) -C t

nginx:
	nginx -c nginx.conf -p . -g 'daemon off;'

serve:
	python -m http.server

clean:
	$(RM) index.html
