package com.github.skozlov.math.arithmetic

import Implicits.longToRational

case class Complex(realPart: Real, imaginaryPart: Real) {
  val isReal: Boolean = imaginaryPart == 0.r

  override def toString: String = {
    if (isReal) {
      realPart.toString
    }
    else if (this == -i) {
      "-i"
    }
    else {
      val real: Option[String] = Some(realPart) filter { _ != 0.r } map {
        case r: Rational => r.toString
        case r => s"($r)"
      }
      val imgMinus: Option[String] = imaginaryPart match {
        case img: Rational if img.sign == -1 => Some("-")
        case _ => None
      }
      val imgFactor: Option[String] = imgMinus
        .map { _ => imaginaryPart.abs }
        .orElse(Some(imaginaryPart))
        .filter { _ != 1.r }
        .map {
          case r: Rational => r.toString
          case r => s"($r)"
        }
      val imgSign: Option[String] = imgMinus orElse (real map { _ => "+" })
      Seq(real, imgSign, imgFactor, Some("i")).flatten mkString " "
    }
  }

  def unary_- : Complex = Complex(-realPart, -imaginaryPart)

  def +(that: Complex): Complex = Complex(
    this.realPart + that.realPart,
    this.imaginaryPart + that.imaginaryPart,
  )

  def -(that: Complex): Complex = this + -that

  def *(that: Complex): Complex = Complex(
    this.realPart * that.realPart - this.imaginaryPart * that.imaginaryPart,
    this.realPart * that.imaginaryPart + this.imaginaryPart * that.realPart,
  )

  def /(that: Complex): Complex = {
    val factor =
      1.r / (that.realPart * that.realPart + that.imaginaryPart * that.imaginaryPart)
    Complex(
      factor * (this.realPart * that.realPart + this.imaginaryPart * that.imaginaryPart),
      factor * (this.imaginaryPart * that.realPart - this.realPart * that.imaginaryPart),
    )
  }
}

object Complex {
  def apply(r: Real): Complex = Complex(r, 0)
}
