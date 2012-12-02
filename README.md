Dependencies:

- Scala 2.9.2
- sbt 0.12.0

Setup:

- Install homebrew(https://github.com/mxcl/homebrew):
  - ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"

- Install scala:
  - brew install scala --with-docs

- Install sbt:
  - brew install sbt

Build and compile:

> $: cd `git-root`/
> $: sbt
> clean
> compile
> test
