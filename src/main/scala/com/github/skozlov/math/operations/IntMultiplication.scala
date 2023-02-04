package com.github.skozlov.math.operations

object IntMultiplication extends ((BigInt, BigInt) => BigInt) {
    override def apply(a: BigInt, b: BigInt): BigInt = a * b
}
