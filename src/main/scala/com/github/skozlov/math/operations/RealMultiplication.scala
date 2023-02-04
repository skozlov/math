package com.github.skozlov.math.operations

import com.github.skozlov.math.numbers.Real

object RealMultiplication extends ((Real, Real) => Real){
    override def apply(a: Real, b: Real): Real = a * b
}
