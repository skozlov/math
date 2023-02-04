package com.github.skozlov.math.operations

import com.github.skozlov.math.numbers.Rational

object RationalMultiplication extends ((Rational, Rational) => Rational){
    override def apply(a: Rational, b: Rational): Rational = a * b
}
