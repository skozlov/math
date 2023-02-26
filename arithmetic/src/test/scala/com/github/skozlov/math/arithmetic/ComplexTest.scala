package com.github.skozlov.math.arithmetic

import com.github.skozlov.commons.scala.test.Test
import com.github.skozlov.math.arithmetic.Implicits._

class ComplexTest extends Test {
  "isReal" should "work as expected" in {
    Complex(0, 0).isReal shouldBe true
    Complex(1, 0).isReal shouldBe true
    Complex(0, 1).isReal shouldBe false
    Complex(1, 1).isReal shouldBe false
  }

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
    Complex(1.r / 2).toString shouldBe "1/2"
    Complex(-1.r / 2).toString shouldBe "-1/2"
    Complex(π).toString shouldBe "π"
    Complex(-π).toString shouldBe "-(π)"
    i.toString shouldBe "i"
    (-i).toString shouldBe "-i"
    (i / 2.r).toString shouldBe "1/2 i"
    (i / -2.r).toString shouldBe "- 1/2 i"
    Complex(0, π).toString shouldBe "(π) i"
    Complex(0, -π).toString shouldBe "(-(π)) i"
    Complex(1.r / 2, 1).toString shouldBe "1/2 + i"
    Complex(1.r / 2, -1).toString shouldBe "1/2 - i"
    Complex(1.r / 2, 1.r / 3).toString shouldBe "1/2 + 1/3 i"
    Complex(-1.r / 2, 1.r / 3).toString shouldBe "-1/2 + 1/3 i"
    Complex(1.r / 2, -1.r / 3).toString shouldBe "1/2 - 1/3 i"
    Complex(-1.r / 2, -1.r / 3).toString shouldBe "-1/2 - 1/3 i"
    Complex(π, e).toString shouldBe "(π) + (e) i"
    Complex(-π, e).toString shouldBe "(-(π)) + (e) i"
    Complex(π, -e).toString shouldBe "(π) + (-(e)) i"
    Complex(-π, -e).toString shouldBe "(-(π)) + (-(e)) i"
  }
}
