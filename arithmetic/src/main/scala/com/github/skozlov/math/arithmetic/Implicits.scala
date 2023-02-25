package com.github.skozlov.math.arithmetic

trait Implicits {
  implicit def bigIntToRational(int: BigInt): Rational = Rational(int)

  implicit def longToRational(n: Long): Rational = Rational(n)

  implicit def stringToRational(s: String): Rational = Rational(s)

  implicit def rationalToRealApproximationBound(
      r: Rational
  ): Real.Approximation.RationalBound = {
    Real.Approximation.RationalBound(r)
  }

  implicit def realToComplex(r: Real): Complex = Complex(r)
}

object Implicits extends Implicits
