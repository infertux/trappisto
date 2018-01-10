# Trappisto

[![Build Status](https://gitlab.com/infertux/trappisto/badges/master/build.svg)](https://gitlab.com/infertux/trappisto/commits/master)

Trappisto is a block explorer designed primarily to explore the
[Decred](https://www.decred.org) blockchain. It aims to:

- have a simple interface
- be mobile-friendly
- always display the latest data
  (the page is refreshed automatically thanks to
  web sockets)
- be fully usable with keyboard shortcuts only (no mouse required)
  (_work in progress_)
- not drown the user with too much technical information
  (a handy link to [dcrdata.org](https://explorer.dcrdata.org)
  is available if you need more details)

## Support for other coins than DCR

Trappisto has experimental support for BTC and BCH. Both coins provide a
similar JSON RPC API with the exception of the `searchrawtransactions` command.
This means it is not possible to browse addresses as we have no way to retrieve
transactions for a particular address. However it is possible to explore blocks
and transactions.

To configure Trappisto for a different coin, you will need to:

- build it with `make trappisto-btc` or `make trappisto-bch` instead of `make`
- update `proxy_pass https://localhost:9109/;` in _nginx.conf_

## What does Trappisto mean?

[TRAPPIST-1](http://www.trappist.one/#about) is a planetary system located 12
parsecs away from the Solar system. But before we get to explore another
planetary system, we can explore our very own blockchain in the Solar system
and since the number zero is preceding one:
`TRAPPIST-1 -> TRAPPIST-0 -> TRAPPIST0 -> Trappist0 -> Trappisto`.
Yeah I know.

## Development

Dependencies:

```
npm i -g elm
npm i -g elm-test
npm i -g uglify-js
```

Build:

```
make
```

Test:

```
make test
```

Run:

```
make dcrd
make nginx
```

## License

AGPLv3+
