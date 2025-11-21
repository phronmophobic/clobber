# clobber

A text editor. Built for [easel](https://github.com/phronmophobic/easel/), but designed to be embeddable in a variety of environments.

Status: alpha

See https://youtu.be/kRd4JYIiWb0?si=SBLUkxu_ptnFB2h9 for a design overview.

## Design goals
- Full featured code and text editor
- First class `clojure-mode`
- Idiomatic clojure API
- Persistent, Immutable data representation
- Full unicode support 😄🤩😜🚀🍩
- Headless-mode (ie. UI entirely decoupled and optional)
- Support additional modes in the future for all your text editing needs

## Rationale

The connection between a lisp developer's program and dev tools is critical. However, for clojure programmers, the editing environment tends to be "over there" in another language, in another runtime, trapped in an OO paradigm. The goal of clobber is to bring text and code editing into the clojure world.

## "Roadmap"

- [X] clojure-mode
- [X] Make instarepl evaluate namespace incrementally
- [ ] formatted-text mode
- [X] Status bar with line/column, file, ns info
- [ ] Improve file open UI
- [X] Implement search backward and improve search UI
- [X] text mode
- [-] markdown mode
- [ ] org mode
- [ ] Files with long lines
- [-] Add generative tests
- [ ] rename editor functions using consistent naming scheme
- [X] refactor and simplify ui code
- [X] long files

## License

Clobber is licensed under Apache License v2.0.
