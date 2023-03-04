# Math

Some math-related Scala code written just for fun.

## Features

### Strict Arithmetic

[Rational numbers](https://en.wikipedia.org/wiki/Rational_number):
https://github.com/skozlov/math/blob/24e28a6fefb4de33a78c83fc747e65ec851425be/demo/src/main/scala/com/github/skozlov/math/demo/arithmetic/quickstart/rational.sc#L1-L9

[Real numbers](https://en.wikipedia.org/wiki/Real_number) with arbitrary
precision:
https://github.com/skozlov/math/blob/8777a861731175837163bd45b88c198d24cb25d1/demo/src/main/scala/com/github/skozlov/math/demo/arithmetic/quickstart/real.sc#L1-L4

[Complex numbers](https://en.wikipedia.org/wiki/Complex_number):
https://github.com/skozlov/math/blob/8777a861731175837163bd45b88c198d24cb25d1/demo/src/main/scala/com/github/skozlov/math/demo/arithmetic/quickstart/complex.sc#L1-L4

### Algebraic Structures

Declare basic properties of operations,
and when you combine them into algebraic structures,
the Scala compiler will check that the operations satisfy the axioms of those
structures.

For example,
if you declare that the string concatenation is associative and has an identity
element,
then you can use it as a monoid:
https://github.com/skozlov/math/blob/8777a861731175837163bd45b88c198d24cb25d1/demo/src/main/scala/com/github/skozlov/math/demo/algebra/quickstart/stringConcat.sc#L1-L25

## Contributing

Prerequisites:

- [Git](https://git-scm.com/).
- JDK 17 (have not tested with other versions).
- [SBT](https://www.scala-sbt.org/) 1.x.

How to contribute:

1. Make the necessary changes in your fork of this repository.
2. Make sure that the code is covered by tests. 100% coverage is required. The
   code which may be uncovered should be enclosed in special comments:

```scala
// $COVERAGE-OFF$
throw new RuntimeException("This code should never be executed")
// $COVERAGE-ON$
```

3. Format Scala and SBT files (`sbt scalafmtAll scalafmtSbt`).
4. Verify the code (`sbt build`).
5. Make sure that the branch is clean:
    - typically, contains one commit with a clear message,
    - contains no merge commits.
6. Create a pull request.
7. Make sure that the CI pipeline is green.

### Contributing with IntelliJ IDEA

Prerequisites (besides common ones described above):

- [IntelliJ IDEA](https://www.jetbrains.com/idea/) (Community or Ultimate
  Edition).
- Recommended plugins:
    - Git.
    - Scala.
- Project SDK: JDK 17.

How to make IDEA use [.scalafmt.conf](.scalafmt.conf) to reformat the code:

1. `File` -> `Settings` -> `Editor` -> `Code Style` -> `Scala`.
2. `Formatter`: `Scalafmt`.
3. `Configuration`: `./.scalafmt.conf` (default).
