package com.github.skozlov.math.algebras

import com.github.skozlov.math.operations.id.Id
import com.github.skozlov.math.operations.inversion.{FullInversion, NonZeroInversion}

trait Checks extends com.github.skozlov.math.operations.Checks {
    def checkMonoidOnTriples[A, Op <: (A, A) => A]
    (op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
    (operands: (A, A, A)*)
    (implicit monoid: Monoid[A, Op]): Unit ={
        implicit val id: Id[A, Op] = monoid.id
        checkAssociativityOnTriples(op, checkEquality)(operands :_*)
        checkId(op, checkEquality)(flattenTriples(operands) :_*)
    }

    def checkMonoid[A, Op <: (A, A) => A]
    (op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
        (operands: A*)
        (implicit monoid: Monoid[A, Op]): Unit = {
        checkMonoidOnTriples(op, checkEquality)(operandsToTriples(operands) :_*)
    }

    def checkGroupOnTriples[A, Op <: (A, A) => A]
    (op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
        (operands: (A, A, A)*)
        (implicit group: Group[A, Op]): Unit = {
        implicit val id: Id[A, Op] = group.id
        implicit val inversion: FullInversion[A, Op] = group.inversion
        checkMonoidOnTriples(op, checkEquality)(operands :_*)
        checkFullInversion(op, checkEquality)(flattenTriples(operands) :_*)
    }

    def checkGroup[A, Op <: (A, A) => A]
    (op: Op, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
        (operands: A*)
        (implicit group: Group[A, Op]): Unit = {
        checkGroupOnTriples(op, checkEquality)(operandsToTriples(operands) :_*)
    }

    def checkRingOnTriples[A, Add <: (A, A) => A, Mul <: (A, A) => A]
    (add: Add, mul: Mul, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
    (operands: (A, A, A)*)
    (implicit ring: Ring[A, Add, Mul]): Unit ={
        implicit val additionGroup: Group[A, Add] = ring.additionGroup
        checkGroupOnTriples(add, checkEquality)(operands :_*)
        checkCommutativity(add, checkEquality)(flattenTriples(operands) :_*)
        checkAssociativityOnTriples(mul, checkEquality)(operands :_*)
        checkDistributivityOnTriples(add, mul, checkEquality)(operands :_*)
    }

    def checkFieldOnTriples[A, Add <: (A, A) => A, Mul <: (A, A) => A]
    (add: Add, mul: Mul, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
    (operands: (A, A, A)*)
    (implicit field: Field[A, Add, Mul]): Unit = {
        implicit val additionId: Id[A, Add] = field.additionGroup.id
        implicit val multiplicationId: Id[A, Mul] = field.multiplicationId
        implicit val multiplicationInversion: NonZeroInversion[A, Mul, Add] = field.multiplicationInversion
        checkRingOnTriples(add, mul, checkEquality)(operands :_*)
        checkCommutativity(mul, checkEquality)(flattenTriples(operands) :_*)
        checkId(mul, checkEquality)(flattenTriples(operands) :_*)
        checkNonZeroInversion[A, Mul, Add](mul, checkEquality)(flattenTriples(operands) :_*)
    }

    def checkField[A, Add <: (A, A) => A, Mul <: (A, A) => A]
    (add: Add, mul: Mul, checkEquality: (A, A) => Any = { (a: A, b: A) => a shouldBe b })
    (operands: A*)
    (implicit field: Field[A, Add, Mul]): Unit = {
        checkFieldOnTriples(add, mul, checkEquality)(operandsToTriples(operands) :_*)
    }
}
