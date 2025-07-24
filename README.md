# clobber

A text editor

Status: alpha

See https://youtu.be/kRd4JYIiWb0?si=SBLUkxu_ptnFB2h9 for a design overview.

## Design goals
- Full featured code and text editor
- First class `clojure-mode`
- Idiomatic clojure API
- Full unicode support 😄🤩😜🚀🍩
- Headless-mode (ie. UI entirely decoupled and optional)
- Support additional modes in the future for all your text editing needs

## Rationale

The connection between a lisp developer's program and dev tools is critical. However, for clojure programmers, the editing environment tends to be "over there" in another language, in another runtime, trapped in an OO paradigm. The goal of clobber is to bring text and code editing into the clojure world.

## "Roadmap"

- [X] clojure-mode
- [ ] formatted-text mode
- [-] text mode
- [-] markdown mode
- [ ] org mode
- [ ] Files with long lines
- [-] Add generative tests
- [ ] rename editor functions using consistent naming scheme
- [ ] refactor and simplify ui code
- [X] long files

## License

Clobber is licensed under Apache License v2.0.
