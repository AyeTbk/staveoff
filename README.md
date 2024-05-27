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
- `make dist` produces a release build and packages it in `dist.zip`, ready to be uploaded to itch.

If you are on Windows, you'll have to adapt the commands on your own to get things to work.

## Post-mortem
This is the first time I use a Lisp and the first time I write anything significant in a functional style. I chose ClojureScript for its ability to compile down to JavaScript, which allows having a web version for the game, which is always nice for game jams.

Ultimately I didn't really enjoy using ClojureScript to develop this game. The language is diametrically opposed to how I like programming languages: static and procedural, as opposed to dynamic and functional-style. The alien syntax is annoying to work with, though one thing I regret a bit is that I didn't write any macros, which I read is probably the only redeeming factor about it. I had a bit of fun at the beginning of the jam getting accustomed to all the weirdness but by the halfway point, I found myself not wanting to implement any new feature and try to write as little code as possible because it felt like a chore to program anything. Near the end of the jam, the only fun I had was implementing the particle system. It's very possible that my half-baked architecture was ill suited for functional programming, so I'd be interested to try making another game in another functional language. The repl driven approach sounded nice at first, but I feel like my tooling (VS Code & the cljs repl) just wasn't good enough for it because it felt unwieldy. I tried to lean hard on what I understand is the Clojure way of doing things, by using maps for most things. Multimethods piqued my interest and I feel like the data-driven approach Clojure goes for works better than the traditional object soup for dynamic languages. However, I don't think I'll be writing any more ClojureScript or Clojure going forward.
