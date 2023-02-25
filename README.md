# Math

Some math-related Scala code written just for fun.

## Contributing

Prerequisites:
- [Git](https://git-scm.com/).
- JDK 17 (have not tested with other versions).
- [SBT](https://www.scala-sbt.org/) 1.x.

How to contribute:
1. Make the necessary changes in your fork of this repository.
2. Make sure that the code is covered by tests. 100% coverage is required.
3. Format Scala and SBT files (`sbt scalafmtAll scalafmtSbt`).
4. Verify the code (`sbt build`).
5. Make sure that the branch is clean:
    - typically, contains one commit with a clear message,
    - contains no merge commits.
6. Create a pull request.
7. Make sure that the CI pipeline is green.

### Contributing with IntelliJ IDEA

Prerequisites (besides common ones described above):
- [IntelliJ IDEA](https://www.jetbrains.com/idea/) (Community or Ultimate Edition).
- Recommended plugins:
  - Git.
  - Scala.
- Project SDK: JDK 17.

How to make IDEA use [.scalafmt.conf](.scalafmt.conf) to reformat the code:
1. `File` -> `Settings` -> `Editor` -> `Code Style` -> `Scala`.
2. `Formatter`: `Scalafmt`.
3. `Configuration`: `./.scalafmt.conf` (default).
