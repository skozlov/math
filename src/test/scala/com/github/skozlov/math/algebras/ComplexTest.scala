package com.github.skozlov.math.algebras

import com.github.skozlov.math.algebras.Implicits._
import com.github.skozlov.math.numbers.Complex
import com.github.skozlov.math.operations.{ComplexAddition, ComplexMultiplication}

class ComplexTest extends Test{
    "Complex numbers" should "be a field" in {
        checkField[Complex, ComplexAddition.type, ComplexMultiplication.type](
            ComplexAddition, ComplexMultiplication)(Complex(1, 2), Complex(3, 4))
    }
}
