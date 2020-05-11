# erlang-pratt

An Erlang implementation of the Pratt parser in
[Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
by [@matklad](https://github.com/matklad).

## Build

Erlang and Rebar3 are required.

```sh
$ rebar3 escriptize
```

## Run

```sh
$ echo "1 + 2 * 3" | _build/default/bin/pratt
(+ 1 (* 2 3))
```

## Test

```sh
$ rebar3 eunit
```

## License

Released under the MIT License (see `LICENSE`).
