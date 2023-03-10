package com.github.skozlov.math.arithmetic

import com.github.skozlov.commons.scala.test.Test

//noinspection RedundantDefaultArgument
class RationalTest extends Test {
  import Implicits._

  "Rational(numerator: BigInt, denominator: BigInt)" should "construct a normalized rational number" in {
    def check(
        r: Rational,
        expectedNumerator: BigInt,
        expectedDenominator: BigInt,
    ): Unit = {
      (
        r.numerator,
        r.denominator,
      ) shouldBe (expectedNumerator, expectedDenominator)
    }

    val ints: Seq[BigInt] = (1 to 6) map { BigInt(_) }
    intercept[ArithmeticException] {
      Rational(0, 0)
    }.getMessage shouldBe "Division by zero"
    for (n <- ints) {
      intercept[ArithmeticException] {
        Rational(n, 0)
      }.getMessage shouldBe "Division by zero"
      check(Rational(0, n), 0, 1)
      check(Rational(n, 1), n, 1)
      check(Rational(n, n), 1, 1)
      for (m <- ints) {
        val r = Rational(n, m)
        val (num, den) = (r.numerator, r.denominator)
        num should be > BigInt(0)
        den should be > BigInt(0)
        num should be <= n
        den should be <= m
        (num gcd den) shouldBe 1
        BigDecimal(n) / BigDecimal(num) shouldBe BigDecimal(m) / BigDecimal(den)
        check(Rational(m, n), den, num)
        check(Rational(-n, m), -num, den)
        check(Rational(n, -m), -num, den)
        check(Rational(-n, -m), num, den)
      }
    }
  }

  "longToRational" should "work as expected" in {
    def check(n: Long, expected: Rational): Unit = {
      val actual: Rational = n
      actual shouldBe expected
    }

    check(0, Rational(0, 1))
    check(1, Rational(1, 1))
    check(2, Rational(2, 1))
    check(-2, Rational(-2, 1))
  }

  "bigIntToRational" should "work as expected" in {
    def check(n: BigInt, expected: Rational): Unit = {
      val actual: Rational = n
      actual shouldBe expected
    }

    check(0, Rational(0, 1))
    check(1, Rational(1, 1))
    check(2, Rational(2, 1))
    check(-2, Rational(-2, 1))
  }

  "/" should "work as expected" in {
    Rational(1) / Rational(3) shouldBe Rational(1, 3)
    Rational(-1) / Rational(3) shouldBe Rational(-1, 3)
    Rational(1) / Rational(-3) shouldBe Rational(-1, 3)
    Rational(-1) / Rational(-3) shouldBe Rational(1, 3)
    intercept[ArithmeticException] {
      Rational(1) / Rational(0)
    }.getMessage shouldBe "Division by zero"
  }

  "-r" should "work as expected" in {
    -0.r shouldBe 0.r
    -2.r shouldBe (-2).r
    -(-2.r) shouldBe 2.r
    -(5.r / 2) shouldBe (-5).r / 2
    -((-5.r) / 2) shouldBe 5.r / 2
  }

  "+" should "work as expected" in {
    1.r / 4 + 1.r / 6 shouldBe 5.r / 12
    1.r / 4 + -1.r / 6 shouldBe 1.r / 12
    -1.r / 4 + 1.r / 6 shouldBe -1.r / 12
  }

  "-" should "work as expected" in {
    1.r / 4 - 1.r / 6 shouldBe 1.r / 12
    1.r / 4 - (-1.r / 6) shouldBe 5.r / 12
    -1.r / 4 - 1.r / 6 shouldBe -5.r / 12
  }

  "*" should "work as expected" in {
    1.r / 3 * 3 shouldBe 1.r
    -1.r / 3 * 3 shouldBe -1.r
    1.r / 3 * -3 shouldBe -1.r
    -1.r / 3 * -3 shouldBe 1.r
  }

  "inversion" should "work as expected" in {
    1.inverted shouldBe 1.r
    -1.inverted shouldBe -1.r
    2.inverted shouldBe 1.r / 2
    -2.inverted shouldBe -1.r / 2
    1.r / 2.inverted shouldBe 2.r
    -1.r / 2.inverted shouldBe -2.r
    intercept[ArithmeticException] {
      0.inverted
    }.getMessage shouldBe "Division by zero"
  }

  "parsing" should "work" in {
    def checkAbs(source: String, radix: Int, expected: Rational): Unit = {
      for {
        source <- Seq(source.toLowerCase, source.toUpperCase)
        prefix <- Seq("", "00")
        withPrefix = prefix + source
      } {
        Rational(withPrefix, radix) shouldBe expected
        Rational("-" + withPrefix, radix) shouldBe -expected
      }
    }

    def checkInt(source: String, radix: Int, expected: BigInt): Unit = {
      for (suffix <- Seq("", ".00", ".(00)", ".00(00)", "/1", " 0/1")) {
        checkAbs(source + suffix, radix, expected)
      }
    }

    def checkNonRepeating(
        source: String,
        radix: Int,
        expected: Rational,
    ): Unit = {
      for (suffix <- Seq("", "00", "(00)", "00(00)")) {
        checkAbs(source + suffix, radix, expected)
      }
    }

    for {
      radix <- Seq(2, 10, 36)
    } {
      checkInt("0", radix, 0)
      checkInt("1", radix, 1)
    }

    checkInt("11", radix = 2, 3)
    checkInt("1295", radix = 10, 1295)
    checkInt("zz", radix = 36, 1295)

    checkNonRepeating("0.11", radix = 2, 3.r / 4)
    checkNonRepeating("0.99", radix = 10, 99.r / 100)
    checkNonRepeating("0.zz", radix = 36, 1295.r / 1296)

    checkNonRepeating("11.11", radix = 2, 3 + 3.r / 4)
    checkNonRepeating("99.99", radix = 10, 99 + 99.r / 100)
    checkNonRepeating("zz.zz", radix = 36, 1295 + 1295.r / 1296)

    checkAbs("0.(03)", radix = 10, 1.r / 33)
    checkAbs("99.(03)", radix = 10, 99 + 1.r / 33)
    checkAbs("99.00(03)", radix = 10, 99 + 1.r / 3300)
    checkAbs("11.00(01)", radix = 2, 3 + 1.r / 12)
    checkAbs("zz.00(01)", radix = 36, 1295 + 1.r / 1678320)

    checkAbs("98/99", radix = 10, 98.r / 99)
    checkAbs("10/11", radix = 2, 2.r / 3)
    checkAbs("zy/zz", radix = 36, 1294.r / 1295)

    checkAbs("99 98/99", radix = 10, 99 + 98.r / 99)
    checkAbs("11 10/11", radix = 2, 3 + 2.r / 3)
    checkAbs("zz zy/zz", radix = 36, 1295 + 1294.r / 1295)

    intercept[NumberFormatException] { Rational("9", radix = 2) }.getMessage
      .shouldBe("""For input string: "9" under radix 2""")
    intercept[NumberFormatException] { Rational("1 1/1.1") }.getMessage
      .shouldBe("Cannot parse rational number: 1 1/1.1")
    intercept[IllegalArgumentException] {
      Rational("1", radix = 37)
    }.getMessage shouldBe "requirement failed: Radix 37 is out of range Range 2 to 36"
  }

  "stringToRational" should "work" in {
    val r: Rational = "99.99"
    r shouldBe 99 + 99.r / 100
  }

  "sign" should "work as expected" in {
    0.r.sign shouldBe 0
    (1.r / 2).sign shouldBe 1
    (-1.r / 2).sign shouldBe -1
  }

  "isInt" should "work as expected" in {
    for (r <- Seq[Rational](0, 2, -2)) {
      r.isInt shouldBe true
    }
    for (r <- Seq(1.r / 2, (-1).r / 2, 3.r / 2, (-3).r / 2)) {
      r.isInt shouldBe false
    }
  }

  "integerPart" should "work as expected" in {
    def check(r: Rational, expected: BigInt): Unit = {
      r.integerPart shouldBe expected
    }

    check(0, 0)
    check(2, 2)
    check(-2, -2)
    check(1.r / 2, 0)
    check(-1.r / 2, 0)
    check(5.r / 2, 2)
    check(-5.r / 2, -2)
  }

  "fractionalPart" should "work as expected" in {
    0.fractionalPart shouldBe 0.r
    2.fractionalPart shouldBe 0.r
    (-2).fractionalPart shouldBe 0.r
    (1.r / 2).fractionalPart shouldBe 1.r / 2
    (-1.r / 2).fractionalPart shouldBe -1.r / 2
    (5.r / 2).fractionalPart shouldBe 1.r / 2
    (-5.r / 2).fractionalPart shouldBe -1.r / 2
  }

  "toPositional" should "work as expected" in {
    for (radix <- Seq(2, 10, 16)) {
      0.toPositional(radix) shouldBe "0"
    }

    3.toPositional(2) shouldBe "11"
    (-3).toPositional(2) shouldBe "-11"
    99.toPositional(10) shouldBe "99"
    (-99).toPositional(10) shouldBe "-99"
    255.toPositional(16) shouldBe "ff"
    (-255).toPositional(16) shouldBe "-ff"

    (15.r / 4).toPositional(2) shouldBe "11.11"
    (-15.r / 4).toPositional(2) shouldBe "-11.11"
    (9999.r / 100).toPositional(10) shouldBe "99.99"
    (-9999.r / 100).toPositional(10) shouldBe "-99.99"
    (65535.r / 256).toPositional(16) shouldBe "ff.ff"
    (-65535.r / 256).toPositional(16) shouldBe "-ff.ff"

    (19.r / 6).toPositional(2) shouldBe "11.0(01)"
    (-19.r / 6).toPositional(2) shouldBe "-11.0(01)"
    (6931.r / 70).toPositional(10) shouldBe "99.0(142857)"
    (-6931.r / 70).toPositional(10) shouldBe "-99.0(142857)"

    1.toPositional(36) shouldBe "1"
    intercept[IllegalArgumentException] {
      1.toPositional(37)
    }.getMessage shouldBe "requirement failed: Radix 37 is out of range Range 2 to 36"
  }

  "toCommonFraction" should "work as expected" in {
    for (radix <- Seq(2, 10, 16)) {
      0.toCommonFraction(radix) shouldBe "0"
    }
    3.toCommonFraction(2) shouldBe "11"
    (-3).toCommonFraction(2) shouldBe "-11"
    (1.r / 3).toCommonFraction(2) shouldBe "1/11"
    (-1.r / 3).toCommonFraction(2) shouldBe "-1/11"

    1.toCommonFraction(36) shouldBe "1"
    intercept[IllegalArgumentException] {
      1.toCommonFraction(37)
    }.getMessage shouldBe "requirement failed: Radix 37 is out of range Range 2 to 36"
  }

  "toMixedFraction" should "work as expected" in {
    0.toMixedFraction(2) shouldBe "0"
    2.toMixedFraction(2) shouldBe "10"
    (-2).toMixedFraction(2) shouldBe "-10"
    (1.r / 2).toMixedFraction(2) shouldBe "1/10"
    (-1.r / 2).toMixedFraction(2) shouldBe "-1/10"
    (3.r / 2).toMixedFraction(2) shouldBe "1 1/10"
    (-3.r / 2).toMixedFraction(2) shouldBe "-1 1/10"

    1.toMixedFraction(36) shouldBe "1"
    intercept[IllegalArgumentException] {
      1.toMixedFraction(37)
    }.getMessage shouldBe "requirement failed: Radix 37 is out of range Range 2 to 36"
  }

  "toString" should "return the same string as toMixedFraction(10)" in {
    (-3.r / 2).toString shouldBe "-1 1/2"
  }

  "compare" should "work as expected" in {
    Seq[Rational](0, 1, -1, 1.r / 2, -1.r / 2, 1.r / 2).sorted
      .shouldBe(Seq[Rational](-1, -1.r / 2, 0, 1.r / 2, 1.r / 2, 1))
  }

  "abs" should "work as expected" in {
    0.r.abs shouldBe 0.r
    (1.r / 2).abs shouldBe 1.r / 2
    (-1.r / 2).abs shouldBe 1.r / 2
  }

  "floor" should "return the greatest integer which is not greater than this" in {
    0.r.floor shouldBe 0
    2.r.floor shouldBe 2
    (-2).r.floor shouldBe -2
    (4.r / 3).floor shouldBe 1
    (-4.r / 3).floor shouldBe -2
    (5.r / 3).floor shouldBe 1
    (-5.r / 3).floor shouldBe -2
    (3.r / 2).floor shouldBe 1
    (-3.r / 2).floor shouldBe -2
  }

  "ceil" should "return the least integer which is not less than this" in {
    0.r.ceil shouldBe 0
    2.r.ceil shouldBe 2
    (-2).r.ceil shouldBe -2
    (4.r / 3).ceil shouldBe 2
    (-4.r / 3).ceil shouldBe -1
    (5.r / 3).ceil shouldBe 2
    (-5.r / 3).ceil shouldBe -1
    (3.r / 2).ceil shouldBe 2
    (-3.r / 2).ceil shouldBe -1
  }

  "round" should "work as expected" in {
    def check(r: Rational, expected: BigInt): Unit = {
      r.round shouldBe expected
      r round 1 shouldBe expected.r
    }

    check(0, 0)
    check(2, 2)
    check(-2, -2)
    check(4.r / 3, 1)
    check(-4.r / 3, -1)
    check(5.r / 3, 2)
    check(-5.r / 3, -2)
    check(3.r / 2, 2)
    check(-3.r / 2, -2)
  }

  "round(precisionDenominator)" should "work as expected" in {
    0.r round 3.r / 2 shouldBe 0.r
    1.r / 2 round 3.r / 2 shouldBe 0.r
    -1.r / 2 round 3.r / 2 shouldBe 0.r
    15.r / 4 round 3.r / 2 shouldBe 9.r / 2
    -15.r / 4 round 3.r / 2 shouldBe -9.r / 2
    4.r round 3.r / 2 shouldBe 9.r / 2
    -4.r round 3.r / 2 shouldBe -9.r / 2
    9.r / 2 round 3.r / 2 shouldBe 9.r / 2
    -9.r / 2 round 3.r / 2 shouldBe -9.r / 2
    5.r round 3.r / 2 shouldBe 9.r / 2
    -5.r round 3.r / 2 shouldBe -9.r / 2
  }

  "pow" should "work as expected" in {
    0.r ^ 0 shouldBe 1.r
    for (exp <- Set(BigInt(1), BigInt(2), BigInt(Int.MaxValue) + 1)) {
      0 ^ exp shouldBe 0.r
      intercept[ArithmeticException] {
        0.r ^ -exp
      }.getMessage shouldBe "Division by zero"
      1 ^ exp shouldBe 1.r
    }
    1 ^ -(BigInt(Int.MaxValue) + 1) shouldBe 1.r
    -1 ^ BigInt(Int.MaxValue) shouldBe -1.r
    -1 ^ -BigInt(Int.MaxValue) shouldBe -1.r
    -1 ^ (BigInt(Int.MaxValue) + 1) shouldBe 1.r
    -1 ^ -(BigInt(Int.MaxValue) + 1) shouldBe 1.r
    for (r <- Seq(2.r / 3, -2.r / 3)) {
      r ^ 0 shouldBe 1.r
      r ^ 1 shouldBe r
      r ^ -1 shouldBe 1 / r
      r ^ 2 shouldBe r * r
      r ^ -2 shouldBe 1 / (r * r)
    }
    for {
      r <- Seq(2.r, 1.r / 2, -2.r, -1.r / 2)
      exp <- Seq(Int.MaxValue, -Int.MaxValue)
    } {
      intercept[ArithmeticException] {
        r ^ exp
      }.getMessage shouldBe "BigInteger would overflow supported range"
    }
  }
}
