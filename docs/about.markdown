---
layout: page
title: About
permalink: /about/
---

A mini-series of posts evolving a minimalist CSV parser implementation in Scala. This will definitely _not_ result in a
production-level library. (In particular, we'll completely ignore crucial parts of the CSV spec, such as quoting, escaping,
multiline columns, etc., and we won't care much about performance for now.) The main goals are:

- Exploring failure modes/error handling.
- Exploring higher level abstractions.
- Exploring [Scala 3](https://docs.scala-lang.org/).
- Exploring [cats](https://typelevel.org/cats/) and [cats-effect](https://typelevel.org/cats-effect/).
- Bringing all of this together in a small, but still somewhat real-world project.
- ...and for me: Exploring [Jekyll](https://jekyllrb.com/) and [github pages](https://pages.github.com/).

The source code for this series can be found [here](https://github.com/sangamon/nanocsv).
