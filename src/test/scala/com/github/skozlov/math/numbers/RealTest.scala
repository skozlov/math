package com.github.skozlov.math.numbers

import com.github.skozlov.math.commons.test.Test
import com.github.skozlov.math.numbers.Implicits._
import com.github.skozlov.math.numbers.Real.Approximation.{+∞, -∞, Interval, Nan}

//noinspection NonAsciiCharacters
class RealTest extends Test {
    "Iteration through approximations" should "work if approximations converge to a single rational number" in {
        Real("Synthetic", LazyList(
            Nan,
            Interval(-∞, +∞),
            Interval(0.r, 2.r),
            Interval(1.r, 2.r),
            Interval(2.r, 2.r)
        )).approximations.last shouldBe Interval(2.r, 2.r)
    }

    "Iteration through approximations" should "fail if next approximation does not include previous" in {
        intercept[ArithmeticException]{Real("Synthetic", LazyList(Interval(1.r, 2.r), Interval(0.r, 2.r))).approximations.last}
            .getMessage shouldBe "Real number approximation [1, 2] does not include the next approximation [0, 2]"
    }

    "Iteration through approximations" should "fail if the last approximation is not a single rational number" in {
        intercept[ArithmeticException]{Real("Synthetic", LazyList(Interval(1.r, 2.r))).approximations.head}
            .getMessage shouldBe "Last approximation of the real number is not a single rational number but [1, 2]"
    }

    "round" should "work as expected" in {
        π.round shouldBe BigInt(3)
        (-π).round shouldBe BigInt(-3)
        π round 1.r/100000 shouldBe 314159.r/100000
        -π round 1.r/100000 shouldBe -314159.r/100000
        e round 1.r/100000 shouldBe 271828.r/100000
    }

    "withStringRepresentation" should "work as expected" in {
        (π withStringRepresentation "π") should be theSameInstanceAs π
        val renamedPi = π withStringRepresentation "pi"
        renamedPi.toString shouldBe "pi"
        renamedPi round 1.r/100000 shouldBe 314159.r/100000
    }

    "abs" should "work as expected" in {
        π.abs round 1.r/100000 shouldBe 314159.r/100000
        (-π).abs round 1.r/100000 shouldBe 314159.r/100000
        (π - π).abs round 1.r/100000 shouldBe 0.r
        π.abs.toString shouldBe "|(π)|"
        (-π).abs.toString shouldBe "|(-(π))|"
    }

    "+" should "work as expected" in {
        (1 + e) round 1.r/100000 shouldBe 371828.r/100000
        (π + e) round 1.r/100000 shouldBe 585987.r/100000
        (-π + -e) round 1.r/100000 shouldBe -585987.r/100000
        (π + -e) round 1.r/100000 shouldBe 42331.r/100000
        (-π + e) round 1.r/100000 shouldBe -42331.r/100000
        (π + e).toString shouldBe "(π) + (e)"

        val a: Real = 1
        val b: Real = 2
        a + b shouldBe 3.r
    }

    "-" should "work as expected" in {
        (1 - e) round 1.r/100000 shouldBe -171828.r/100000
        (π - e) round 1.r/100000 shouldBe 42331.r/100000
        (e - π) round 1.r/100000 shouldBe -42331.r/100000
        (-π - e) round 1.r/100000 shouldBe -585987.r/100000
        (-e - π) round 1.r/100000 shouldBe -585987.r/100000
        (π - e).toString shouldBe "(π) - (e)"

        val a: Real = 3
        val b: Real = 2
        a - b shouldBe 1.r
    }

    "*" should "work as expected" in {
        (2 * e) round 1.r/100000 shouldBe 543656.r/100000
        (π * e) round 1.r/100000 shouldBe 853973.r/100000
        (-π * e) round 1.r/100000 shouldBe -853973.r/100000
        (π * -e) round 1.r/100000 shouldBe -853973.r/100000
        (-π * -e) round 1.r/100000 shouldBe 853973.r/100000
        (π * (e - (2.r + 3.r/4))) round 1.r/100000 shouldBe -9965.r/100000
        (-π * (e - (2.r + 3.r/4))) round 1.r/100000 shouldBe 9965.r/100000
        ((π - 3) * (e - (2.r + 3.r/4))) round 1.r/100000 shouldBe -449.r/100000
        (π * e).toString shouldBe "(π) * (e)"

        val a: Real = 3
        val b: Real = 2
        a * b shouldBe 6.r
    }

    "/" should "work as expected" in {
        (1 / e) round 1.r/100000 shouldBe 36788.r/100000
        (π / 2) round 1.r/100000 shouldBe 15708.r/10000
        (2 / π) round 1.r/100000 shouldBe 63662.r/100000
        (π / e) round 1.r/100000 shouldBe 115573.r/100000
        (-π / e) round 1.r/100000 shouldBe -115573.r/100000
        (π / -e) round 1.r/100000 shouldBe -115573.r/100000
        (-π / -e) round 1.r/100000 shouldBe 115573.r/100000
        intercept[ArithmeticException]{(π / 0) round 1.r/10}.getMessage shouldBe "Division by zero"
        (1 / (π - π)).approximations.take(10) shouldBe Seq.fill(10){Nan}
        (0.r - -(1.r / (π - 3)) * 1 + 0) round 1.r/100000 shouldBe 706251.r/100000
        (1.r / (1.r / (π - 3))) round 1.r/100000 shouldBe 14159.r/100000
        (π / e).toString shouldBe "(π) / (e)"

        val a: Real = 3
        val b: Real = 2
        a / b shouldBe 3.r/2
    }

    "pow" should "work as expected" in {
        for (r <- Seq(π, -π, π - π)){
            r ^ 0 shouldBe 1.r
            r ^ 1 shouldBe r
        }
        for (r <- Seq(π, -π)) {
            (r ^ 2) round 1.r / 100000 shouldBe 986960.r / 100000
            (r ^ -2) round 1.r / 100000 shouldBe 10132.r / 100000
        }
        (π ^ 3) round 1.r / 100000 shouldBe 3100628.r / 100000
        (-π ^ 3) round 1.r / 100000 shouldBe -3100628.r / 100000
        (π ^ -3) round 1.r / 100000 shouldBe 3225.r / 100000
        (-π ^ -3) round 1.r / 100000 shouldBe -3225.r / 100000
        ((π - π) ^ -1).approximations.take(10) shouldBe Seq.fill(10){Nan}
        (π ^ 2).toString shouldBe "(π) ^ 2"

        val r: Real = 2
        r ^ 2 shouldBe 4.r
    }
}
