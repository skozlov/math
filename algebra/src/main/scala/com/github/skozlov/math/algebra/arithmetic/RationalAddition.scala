package com.github.skozlov.math.algebra.arithmetic

import com.github.skozlov.math.arithmetic.Rational

object RationalAddition extends ((Rational, Rational) => Rational){
    override def apply(a: Rational, b: Rational): Rational = a + b
}
