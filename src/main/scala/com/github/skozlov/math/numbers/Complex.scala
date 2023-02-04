package com.github.skozlov.math.numbers

import Implicits.longToRational

case class Complex(realPart: Real, imaginaryPart: Real){
    override def toString: String = s"($realPart) + i * ($imaginaryPart)"

    def unary_- : Complex = Complex(-realPart, -imaginaryPart)

    def +(that: Complex): Complex = Complex(this.realPart + that.realPart, this.imaginaryPart + that.imaginaryPart)

    def -(that: Complex): Complex = this + -that

    def *(that: Complex): Complex = Complex(
        this.realPart * that.realPart - this.imaginaryPart * that.imaginaryPart,
        this.realPart * that.imaginaryPart + this.imaginaryPart * that.realPart
    )

    def /(that: Complex): Complex = {
        val factor = 1.r / (that.realPart * that.realPart + that.imaginaryPart * that.imaginaryPart)
        Complex(
            factor * (this.realPart * that.realPart + this.imaginaryPart * that.imaginaryPart),
            factor * (this.imaginaryPart * that.realPart - this.realPart * that.imaginaryPart)
        )
    }
}

object Complex{
    def apply(r: Real): Complex = Complex(r, 0)
}
