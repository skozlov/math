package com.github.skozlov.math.algebra.arithmetic

object IntMultiplication extends ((BigInt, BigInt) => BigInt) {
    override def apply(a: BigInt, b: BigInt): BigInt = a * b
}
