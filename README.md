# Stave Off
This is my entry for the [Spring Lisp Game Jam 2024](https://itch.io/jam/spring-lisp-game-jam-2024).

Playable [here](https://ayetbk.itch.io/stave-off).

Stave Off is a Breakout game with a twist: you have to survive, there are no levels and the longer
you've been playing, the harder it gets (well that was the plan, anyway).

## How to run or build
First you need ClojureScript ([quick start](https://clojurescript.org/guides/quick-start)).

Then you can use the targets in the Makefile:
- `make repl` lauches a browser running the game and starts a repl connected to it.
- `make release` produces a release build in ./out/main.js.
- `make serve` launches a local web server that serves the release build.

If you are on Windows, you'll have to adapt the commands on your own to get things to work.

## Post-mortem
This is the first time I use a Lisp and the first time I write anything significant
in a functional style. I chose ClojureScript for its ability to compile down to
JavaScript, which allows having a web version for the game, which is always nice for game jams.

TODO write more about your experience
