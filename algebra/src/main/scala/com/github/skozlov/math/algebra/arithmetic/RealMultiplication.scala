package com.github.skozlov.math.algebra.arithmetic

import com.github.skozlov.math.arithmetic.Real

object RealMultiplication extends ((Real, Real) => Real){
    override def apply(a: Real, b: Real): Real = a * b
}
