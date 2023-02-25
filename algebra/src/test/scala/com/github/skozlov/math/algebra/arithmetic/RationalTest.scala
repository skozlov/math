package com.github.skozlov.math.algebra.arithmetic

import com.github.skozlov.math.algebra.Test
import com.github.skozlov.math.arithmetic.Rational

class RationalTest extends Test {
  "Rational numbers" should "be a field" in {
    checkField[Rational, RationalAddition.type, RationalMultiplication.type](
      RationalAddition,
      RationalMultiplication,
    )(1.r / 2, 1.r / 3)
  }
}
