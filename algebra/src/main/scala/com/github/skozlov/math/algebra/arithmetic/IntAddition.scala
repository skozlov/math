package com.github.skozlov.math.algebra.arithmetic

object IntAddition extends ((BigInt, BigInt) => BigInt) {
    override def apply(a: BigInt, b: BigInt): BigInt = a + b
}
