package com.github.skozlov.math.algebras

import com.github.skozlov.math.algebras.Implicits._
import com.github.skozlov.math.numbers.{Real, e, π}
import com.github.skozlov.math.operations.{RealAddition, RealMultiplication}

class RealTest extends Test{
    private def checkEquality(a: Real, b: Real): Unit = {
        val precision = 1.r/1000000
        a.round(precision) shouldBe b.round(precision)
    }

    "Real numbers" should "be a field" in {
        checkField(RealAddition, RealMultiplication, checkEquality)(2, π, e)
    }
}
