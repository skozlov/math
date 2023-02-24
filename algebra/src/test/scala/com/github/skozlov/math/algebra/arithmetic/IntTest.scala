package com.github.skozlov.math.algebra.arithmetic

import com.github.skozlov.math.algebra.Test

class IntTest extends Test{
    "Integer numbers" should "be an abelian group under addition" in {
        val op = IntAddition
        val operands: Seq[BigInt] = Seq(2, 3)
        checkCommutativity(op)(operands :_*)
        checkGroup[BigInt, IntAddition.type](op)(operands :_*)
    }

    "Integer numbers" should "be a commutative monoid under multiplication" in {
        val op = IntMultiplication
        val operands: Seq[BigInt] = Seq(2, 3)
        checkCommutativity(op)(operands: _*)
        checkMonoid[BigInt, IntMultiplication.type](op)(operands: _*)
    }

    "Addition and multiplication" should "be distributive on integer numbers" in {
        checkDistributivity[BigInt, IntAddition.type, IntMultiplication.type](
            IntAddition, IntMultiplication)(2, 3)
    }
}
