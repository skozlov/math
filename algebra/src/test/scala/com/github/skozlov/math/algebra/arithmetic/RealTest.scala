package com.github.skozlov.math.algebra.arithmetic

import com.github.skozlov.math.algebra.Test
import com.github.skozlov.math.arithmetic.{Real, e, π}

class RealTest extends Test {
  private def checkEquality(a: Real, b: Real): Unit = {
    val precision = 1.r / 1000000
    a.round(precision) shouldBe b.round(precision)
  }

  "Real numbers" should "be a field" in {
    checkField(RealAddition, RealMultiplication, checkEquality)(2, π, e)
  }
}
