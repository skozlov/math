package com.github.skozlov.math.operations

import com.github.skozlov.math.operations.id.{Id, LeftId, RightId}
import com.github.skozlov.math.operations.inversion.{FullInversion, NonZeroInversion, PartialInversion}
import org.scalatest.matchers.should.Matchers

trait Checks extends Matchers{
    def operandsToTriples[A](operands: Seq[A]): Seq[(A, A, A)] = {
        val ops = operands.distinct
        for {
            a <- ops
            b <- ops
            c <- ops
        } yield (a, b, c)
    }

    def flattenTriples[A](operands: Seq[(A, A, A)]): Seq[A] = {
        (operands flatMap {case (a, b, c) => Seq(a, b, c)}).distinct
    }

    def checkCommutativity[A, B, Op <: (A, A) => B]
    (op: Op, checkEquality: (B, B) => Any = { (a: B, b: B) => a shouldBe b })
        (operands: A*): Unit = {
        val ops = operands.distinct
        for {
            j <- ops.indices
            k <- ops.indices if k >= j
        } {
            val a = ops(j)
            val b = ops(k)
            checkEquality(op(a, b), op(b, a))
        }
    }

    def checkAssociativityOnTriples[A, Op <: (A, A) => A]
    (op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
        (operands: (A, A, A)*): Unit = {
        val ops = operands.distinct
        for {
            (a, b, c) <- ops
        } {
            checkEquality(op(op(a, b), c), op(a, op(b, c)))
        }
    }

    def checkAssociativity[A, Op <: (A, A) => A]
    (op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
    (operands: A*): Unit = {
        checkAssociativityOnTriples(op, checkEquality)(operandsToTriples(operands) :_*)
    }

    def checkLeftId[A, B, Op <: (A, B) => B](
        op: Op, checkEquality: (B, B) => Any = { (a: B, b: B) => a shouldBe b }
    )(operands: B*)(implicit id: LeftId[A, B, Op]): Unit ={
        for (b <- operands) {
            checkEquality(b, op(id.leftId, b))
        }
    }

    def checkRightId[A, B, Op <: (A, B) => A](
        op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b }
    )(operands: A*)(implicit id: RightId[A, B, Op]): Unit ={
        for (a <- operands) {
            checkEquality(a, op(a, id.rightId))
        }
    }

    def checkId[A, Op <: (A, A) => A](
        op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b }
    )(operands: A*)(implicit id: Id[A, Op]): Unit ={
        checkLeftId(op, checkEquality)(operands :_*)
        checkRightId(op, checkEquality)(operands :_*)
    }

    def checkPartialInversion[A, Op <: (A, A) => A]
    (op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
    (operands: A*)
    (nonInvertible: A*)
    (implicit id: Id[A, Op], inversion: PartialInversion[A, Op])
    : Unit = {
        val invert = inversion.invertPartial
        val _nonInvertible = Set(nonInvertible :_*)
        for (a <- operands) {
            if (_nonInvertible contains a){
                invert isDefinedAt a shouldBe false
            }
            else{
                val inverted = invert(a)
                checkEquality(id.id, op(a, inverted))
                checkEquality(id.id, op(inverted, a))
            }
        }
    }

    def checkNonZeroInversion[A, Op <: (A, A) => A, ZeroOp <: (A, A) => A]
    (op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
    (operands: A*)
    (implicit id: Id[A, Op], zero: Id[A, ZeroOp], inversion: NonZeroInversion[A, Op, ZeroOp])
    : Unit = {
        val _zero = zero.id
        checkPartialInversion(op, checkEquality)(operands appended _zero :_*)(_zero)
    }

    def checkFullInversion[A, Op <: (A, A) => A](
        op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b }
    )(operands: A*)(implicit id: Id[A, Op], inversion: FullInversion[A, Op]): Unit ={
        checkPartialInversion(op, checkEquality)(operands:_*)()
    }

    def checkLeftDistributivityOnTriples[A, B, Add <: (B, B) => B, Mul <: (A, B) => B]
    (add: Add, mul: Mul, checkEquality: (B, B) => Any = { (a: B, b: B) => a shouldBe b })
        (operands: (A, B, B)*): Unit = {
        val ops = operands.distinct
        for {
            (a, b, c) <- ops
        } {
            checkEquality(mul(a, add(b, c)), add(mul(a, b), mul(a, c)))
        }
    }

    def checkRightDistributivityOnTriples[A, B, Add <: (A, A) => A, Mul <: (A, B) => A]
    (add: Add, mul: Mul, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
        (operands: (A, A, B)*): Unit = {
        val ops = operands.distinct
        for {
            (a, b, c) <- ops
        } {
            checkEquality(mul(add(a, b), c), add(mul(a, c), mul(b, c)))
        }
    }

    def checkDistributivityOnTriples[A, Add <: (A, A) => A, Mul <: (A, A) => A]
    (add: Add, mul: Mul, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
        (operands: (A, A, A)*): Unit = {
        checkLeftDistributivityOnTriples(add, mul, checkEquality)(operands :_*)
        checkRightDistributivityOnTriples(add, mul, checkEquality)(operands :_*)
    }

    def checkDistributivity[A, Add <: (A, A) => A, Mul <: (A, A) => A]
    (add: Add, mul: Mul, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
        (operands: A*): Unit = {
        checkDistributivityOnTriples(add, mul, checkEquality)(operandsToTriples(operands) :_*)
    }
}
