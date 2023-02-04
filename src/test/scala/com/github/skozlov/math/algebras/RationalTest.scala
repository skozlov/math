package com.github.skozlov.math.algebras

import com.github.skozlov.math.algebras.Implicits._
import com.github.skozlov.math.numbers.Rational
import com.github.skozlov.math.operations.{RationalAddition, RationalMultiplication}

class RationalTest extends Test{
    "Rational numbers" should "be a field" in {
        checkField[Rational, RationalAddition.type, RationalMultiplication.type](
            RationalAddition, RationalMultiplication)(1.r/2, 1.r/3)
    }
}
