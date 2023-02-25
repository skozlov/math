package com.github.skozlov.math.algebra.arithmetic

import com.github.skozlov.math.algebra.Test
import com.github.skozlov.math.arithmetic.Complex

class ComplexTest extends Test {
  "Complex numbers" should "be a field" in {
    checkField[Complex, ComplexAddition.type, ComplexMultiplication.type](
      ComplexAddition,
      ComplexMultiplication,
    )(Complex(1, 2), Complex(3, 4))
  }
}
