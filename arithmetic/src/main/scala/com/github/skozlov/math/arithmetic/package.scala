package com.github.skozlov.math

package object arithmetic {
  import Real.Approximation
  import Implicits._

  val Pi: Real = {
    import Approximation._

    /**
     * sum (-1)^j^ x^2j+1^ / 2j+1
     */
    def arctg(x: Rational): Real = Real(
      s"arctg(x)",
      new IterableOnce[Approximation] {
        require(0.r < x && x < 1.r)
        lazy val x2: Rational = x * x
        override def iterator: Iterator[Approximation] = {
          Iterator.iterate(((0.r, x), x, 1)) { case (interval, num, den) =>
            val nextNum: Rational = -num * x2
            val nextDen: Int = den + 2
            val nextMember = nextNum / nextDen
            val nextInterval = {
              if (nextMember.sign > 0) {
                (interval._1, interval._1 + nextMember)
              }
              else {
                (interval._2 + nextMember, interval._2)
              }
            }
            (nextInterval, nextNum, nextDen)
          } map { case ((min, max), _, _) => Interval(min, max) }
        }
      },
    )

    (arctg(1.r / 5) * 16 - arctg(1.r / 239) * 4) withStringRepresentation "π"
  }

  // noinspection NonAsciiCharacters
  val π: Real = Pi

  val e: Real = {
    Real(
      "e",
      new IterableOnce[Approximation] {
        import Approximation._

        override def iterator: Iterator[Approximation] = Iterator
          .iterate((2, 1.r, (2.r, 3.r))) { case (i, prevMember, prevInterval) =>
            val member = prevMember / i
            val min = prevInterval._1 + member
            val max = min + member
            (i + 1, member, (min, max))
          }
          .map { case (_, _, (min, max)) => Interval(min, max) }
      },
    )
  }

  val i: Complex = Complex(0, 1)
}
