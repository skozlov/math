package com.github.skozlov.math.numbers

import com.github.skozlov.math.commons.test.Test
import com.github.skozlov.math.numbers.Implicits._
import com.github.skozlov.math.numbers.Real.Approximation
import com.github.skozlov.math.numbers.Real.Approximation.{+∞, -∞, Interval, Nan, RationalBound}

class RealApproximationIntervalTest extends Test {
    "toString" should "work" in {
        Interval(-∞, +∞).toString shouldBe "(-∞, +∞)"
        Interval(-∞, 0.r).toString shouldBe "(-∞, 0]"
        Interval(1.r, +∞).toString shouldBe "[1, +∞)"
        Interval(0.r, 1.r).toString shouldBe "[0, 1]"
    }

    "includes" should "work" in {
        def check(approximations: Approximation*): Unit ={
            approximations foreach {i => i.includes(i) shouldBe true}
            approximations reduce {(a, b) =>
                b.includes(a) shouldBe true
                a.includes(b) shouldBe false
                b
            }
        }

        check(
            Interval(0.r, 0.r),
            Interval(-1.r, 1.r),
            Interval(-1.r, 2.r),
            Interval(-2.r, 2.r),
            Interval(-∞, 2.r),
            Interval(-∞, +∞),
            Nan,
        )
        check(Interval(-2.r, 2.r), Interval(-2.r, +∞), Interval(-∞, +∞))
    }

    "round" should "work" in {
        Interval(0.r, 0.r) round 1.r/1000000 shouldBe Some(0.r)
        Interval(0.r, 1.r/2) round 1 shouldBe None
        Interval(0.r, 1.r/3) round 1 shouldBe Some(0.r)
        Interval(0.r, 1.r/3) round 1.r/2 shouldBe None
        Interval(0.r, +∞) round 1 shouldBe None
        Interval(-∞, 0.r) round 1 shouldBe None
        Interval(-∞, +∞) round 1 shouldBe None
    }

    "unary -" should "work" in {
        -Interval(-∞, +∞) shouldBe Interval(-∞, +∞)
        -Interval(-∞, 1.r) shouldBe Interval(-1.r, +∞)
        -Interval(-1.r, +∞) shouldBe Interval(-∞, 1.r)
        -Interval(1.r, 2.r) shouldBe Interval(-2.r, -1.r)
        -Interval(-2.r, -1.r) shouldBe Interval(1.r, 2.r)
        -Interval(-1.r, 2.r) shouldBe Interval(-2.r, 1.r)
        -Interval(1.r, 1.r) shouldBe Interval(-1.r, -1.r)
        -Interval(0.r, 1.r) shouldBe Interval(-1.r, 0.r)
    }

    "abs" should "work" in {
        Interval(-∞, +∞).abs shouldBe Interval(0.r, +∞)
        Interval(-∞, 1.r).abs shouldBe Interval(0.r, +∞)
        Interval(-1.r, +∞).abs shouldBe Interval(0.r, +∞)
        Interval(-∞, -1.r).abs shouldBe Interval(1.r, +∞)
        Interval(1.r, +∞).abs shouldBe Interval(1.r, +∞)
        Interval(-1.r, 2.r).abs shouldBe Interval(0.r, 2.r)
        Interval(-2.r, 1.r).abs shouldBe Interval(0.r, 2.r)
        Interval(1.r, 2.r).abs shouldBe Interval(1.r, 2.r)
        Interval(-2.r, -1.r).abs shouldBe Interval(1.r, 2.r)
        Interval(0.r, 0.r).abs shouldBe Interval(0.r, 0.r)
    }

    "+" should "work" in {
        Interval(1.r, 1.r) + Interval(2.r, 2.r) shouldBe Interval(3.r, 3.r)
        Interval(1.r, 2.r) + Interval(2.r, 2.r) shouldBe Interval(3.r, 4.r)
        Interval(-2.r, 1.r) + Interval(1.r, 2.r) shouldBe Interval(-1.r, 3.r)
        Interval(1.r, 2.r) + Interval(-∞, 3.r) shouldBe Interval(-∞, 5.r)
        Interval(1.r, 2.r) + Interval(3.r, +∞) shouldBe Interval(4.r, +∞)
        Interval(-∞, 1.r) + Interval(-∞, 2.r) shouldBe Interval(-∞, 3.r)
        Interval(1.r, +∞) + Interval(2.r, +∞) shouldBe Interval(3.r, +∞)
        (Interval(-∞, 1.r) + Interval(2.r, +∞)) shouldBe Interval(+∞, -∞)
    }

    "*" should "work" in {
        Interval(2.r, 3.r) * Interval(4.r, 5.r) shouldBe Interval(8.r, 15.r)
        Interval(2.r, 3.r) * Interval(4.r, +∞) shouldBe Interval(8.r, +∞)

        Interval(-3.r, -2.r) * Interval(-5.r, -4.r) shouldBe Interval(8.r, 15.r)
        Interval(-3.r, -2.r) * Interval(-∞, -4.r) shouldBe Interval(8.r, +∞)

        Interval(-3.r, -2.r) * Interval(4.r, 5.r) shouldBe Interval(-15.r, -8.r)
        Interval(-∞, -2.r) * Interval(4.r, +∞) shouldBe Interval(-∞, -8.r)

        Interval(-5.r, -4.r) * Interval(-3.r, 2.r) shouldBe Interval(-10.r, 15.r)
        Interval(-∞, -4.r) * Interval(-3.r, 2.r) shouldBe Interval(-∞, +∞)
        Interval(-5.r, -4.r) * Interval(-∞, 2.r) shouldBe Interval(-10.r, +∞)
        Interval(-5.r, -4.r) * Interval(-3.r, +∞) shouldBe Interval(-∞, 15.r)

        Interval(-2.r, 3.r) * Interval(4.r, 5.r) shouldBe Interval(-10.r, 15.r)
        Interval(-∞, 3.r) * Interval(4.r, 5.r) shouldBe Interval(-∞, 15.r)
        Interval(-2.r, +∞) * Interval(4.r, 5.r) shouldBe Interval(-10.r, +∞)
        Interval(-2.r, 3.r) * Interval(4.r, +∞) shouldBe Interval(-∞, +∞)

        Interval(-2.r, 3.r) * Interval(-4.r, 5.r) shouldBe Interval(-12.r, 15.r)
        Interval(-∞, 3.r) * Interval(-4.r, 5.r) shouldBe Interval(-∞, +∞)
        Interval(-2.r, 3.r) * Interval(-∞, 5.r) shouldBe Interval(-∞, +∞)
        Interval(-2.r, +∞) * Interval(-4.r, 5.r) shouldBe Interval(-∞, +∞)
        Interval(-2.r, 3.r) * Interval(-4.r, +∞) shouldBe Interval(-∞, +∞)

        Interval(0.r, 0.r) * Interval(1.r, +∞) shouldBe Interval(0.r, 0.r)
        Interval(0.r, 0.r) * Interval(-1.r, +∞) shouldBe Interval(0.r, 0.r)
        Interval(0.r, 0.r) * Interval(-∞, -1.r) shouldBe Interval(0.r, 0.r)
        Interval(0.r, 0.r) * Interval(-∞, 1.r) shouldBe Interval(0.r, 0.r)
        Interval(0.r, 0.r) * Interval(-∞, +∞) shouldBe Interval(0.r, 0.r)
    }

    "multiplicativeInverse" should "work" in {
        intercept[ArithmeticException]{Interval(0.r, 0.r).inverted}.getMessage shouldBe "Division by zero"
        Interval(2.r, 2.r).inverted shouldBe Interval(1.r/2, 1.r/2)
        Interval(-2.r, -2.r).inverted shouldBe Interval(-1.r/2, -1.r/2)
        Interval(2.r, 3.r).inverted shouldBe Interval(1.r/3, 1.r/2)
        Interval(-3.r, -2.r).inverted shouldBe Interval(-1.r/2, -1.r/3)
        Interval(2.r, +∞).inverted shouldBe Interval(0.r, 1.r/2)
        Interval(-∞, -2.r).inverted shouldBe Interval(-1.r/2, 0.r)
        Interval(-2.r, 3.r).inverted shouldBe Nan
        Interval(-∞, 3.r).inverted shouldBe Nan
        Interval(-2.r, +∞).inverted shouldBe Nan
        Interval(-∞, +∞).inverted shouldBe Nan
    }

    "pow" should "work" in {
        for (i <- Seq(
            Interval(0.r, 0.r),
            Interval(0.r, 2.r),
            Interval(-2.r, 0.r),
            Interval(1.r, 2.r),
            Interval(-2.r, -1.r),
            Interval(-1.r, 2.r),
            Interval(-∞, -2.r),
            Interval(-∞, 2.r),
            Interval(-2.r, +∞),
            Interval(2.r, +∞),
            Interval(-∞, +∞)
        )) {
            i pow 0 shouldBe Interval(1.r, 1.r)
            i pow 1 shouldBe i
        }
        intercept[ArithmeticException]{Interval(0.r, 0.r) pow -1}.getMessage shouldBe "Division by zero"
        Interval(-∞, -2.r) pow 2 shouldBe Interval(4.r, +∞)
        Interval(-∞, -2.r) pow -2 shouldBe Interval(0.r, 1.r/4)
        Interval(-∞, -2.r) pow 3 shouldBe Interval(-∞, -8.r)
        Interval(-∞, -2.r) pow -3 shouldBe Interval(0.r, -1.r/8)
        Interval(-2.r, 3.r) pow 2 shouldBe Interval(0.r, 9.r)
        Interval(-3.r, 2.r) pow 2 shouldBe Interval(0.r, 9.r)
        Interval(-2.r, 3.r) pow 3 shouldBe Interval(-8.r, 27.r)
        Interval(-∞, +∞) pow 2 shouldBe Interval(0.r, +∞)
        Interval(-∞, +∞) pow 3 shouldBe Interval(-∞, +∞)
        for (b <- Seq(+∞, -∞)){
            b pow -2 shouldBe RationalBound(0.r)
        }
    }
}
