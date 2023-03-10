package com.github.skozlov.math.algebra.arithmetic

import com.github.skozlov.math.arithmetic.Complex

object ComplexAddition extends ((Complex, Complex) => Complex) {
  override def apply(a: Complex, b: Complex): Complex = a + b
}
