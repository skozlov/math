package com.github.skozlov.math.arithmetic

import Implicits._
import com.github.skozlov.commons.scala.test.Test

class ComplexTest extends Test {
  "realToComplex" should "work as expected" in {
    val n: Complex = 1.r
    n shouldBe Complex(1.r, 0.r)
  }

  "unary -" should "work as expected" in {
    -Complex(2, 3) shouldBe Complex(-2, -3)
    -Complex(-2, 3) shouldBe Complex(2, -3)
    -Complex(2, -3) shouldBe Complex(-2, 3)
    -Complex(-2, -3) shouldBe Complex(2, 3)
  }

  "+" should "work as expected" in {
    Complex(2, 3) + Complex(4, 5) shouldBe Complex(6, 8)
  }

  "-" should "work as expected" in {
    Complex(2, 3) - Complex(4, 5) shouldBe Complex(-2, -2)
  }

  "*" should "work as expected" in {
    Complex(2, 3) * Complex(4, 5) shouldBe Complex(-7, 22)
  }

  "/" should "work as expected" in {
    Complex(2, 3) / Complex(4, 5) shouldBe Complex(23.r / 41, 2.r / 41)
  }

  "toString" should "work as expected" in {
    Complex(1, 2).toString shouldBe "(1) + i * (2)"
  }
}
