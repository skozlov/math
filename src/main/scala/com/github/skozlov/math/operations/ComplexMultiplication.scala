package com.github.skozlov.math.operations

import com.github.skozlov.math.numbers.Complex

object ComplexMultiplication extends ((Complex, Complex) => Complex){
    override def apply(a: Complex, b: Complex): Complex = a * b
}
