# lc

[![CircleCI](https://circleci.com/gh/hmac/lc.svg?style=svg)](https://circleci.com/gh/hmac/lc)

lc is a collection of experimental Lambda Calculus evaluators. It's a playground for
exploring different type systems.

Current implementations:

- [x] Untyped Lambda Calculus
- [x] Simply Typed Lambda Calculus
- [x] System T
- [x] System F
- [x] Hindley-Milner
- [x] Dependently Typed Lambda Calculus (λ𝚷)

## Try it out

A minimal UI for playing around with these languages is hosted
[here](https://hmac.dev/lc/index.html).

To run locally, install
[Purescript](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md)
and run `bower install` and `pulp build`. Then open `index.html`.
