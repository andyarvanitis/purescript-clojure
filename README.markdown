[![PureScript](logo.png)](http://purescript.org)

A small strongly typed programming language with expressive types that compiles to Javascript, written in and inspired by Haskell.

## An experimental [Clojure](http://clojure.org/) backend for PureScript

#### Status
* This is a quick, first-pass effort, so not much work has gone into it
* Has been successfully run against a few tests from `examples\passing`

#### Requirements

* Everything you need to build [PureScript](https://github.com/purescript/purescript)
* Clojure via [Leiningen](http://leiningen.org/)
* Versions of the PureScript standard modules. Currently, the following modules have been ported, all found in repo [https://github.com/andyarvanitis](https://github.com/andyarvanitis?tab=repositories):
 * purescript-prelude
 * purescript-eff
 * purescript-console

  Note that you **must use branch `clojure`** for each of these. Also note that they are based on the ongoing 0.8.x module overhaul, so some tests won't work. Any modules that do not require FFI can also be used (or at least tried).

#### Basic instructions
* Build purescript in the usual way, but using code from this repo
* Install [Leiningen](http://leiningen.org/)
* Build your PureScript source whatever way you typically do so, making sure to use the versions of the standard modules mentioned above
* `cd` to your `output` directory
* Execute `lein run` to run your program, `lein repl`, or `lein uberjar` to AOT compile, etc. (normal Clojure stuff)
