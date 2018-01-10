all: trappisto

trappisto: src/*.elm
	elm-make src/Trappisto.elm --yes --warn --output public/assets/js/elm.js
	uglifyjs --mangle --screw-ie8 --output public/assets/js/elm.min.js -- public/assets/js/elm.js
	rm public/assets/js/elm.js

trappisto-btc: trappisto
	cp public/index.html public/index-btc.html
	sed -i 's/DCR/BTC/g' public/index-btc.html
	sed -i 's/Decred/Bitcoin/g' public/index-btc.html

trappisto-bch: trappisto
	cp public/index.html public/index-bch.html
	sed -i 's/DCR/BCH/g' public/index-bch.html
	sed -i 's/Decred/Bitcoin Cash/g' public/index-bch.html

test: src/*.elm tests/*.elm
	elm-test --yes

watch: src/*.elm tests/*.elm
	elm-test --watch

clean:
	$(RM) -r public/assets/js/elm*.js elm-stuff/ tests/elm-stuff/

nginx:
	sudo -u http -- nginx -c nginx/nginx.conf -p . -g 'daemon off;'

dcrd:
	dcrd --rpcuser bitcoin --rpcpass secret

bitcoind:
	bitcoind -par=-1 -server -txindex -rpcuser=bitcoin -rpcpassword=secret

.PHONY: watch clean nginx dcrd
