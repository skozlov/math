package com.github.skozlov.math.arithmetic

import com.github.skozlov.commons.scala.lang.{
  RichMatcher,
  RichString,
  checkNumberFormatRadix,
}

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable.{LinkedHashSet, StringBuilder}

case class Rational private (numerator: BigInt, denominator: BigInt)
    extends Real
    with Ordered[Rational] {
  import Implicits.{bigIntToRational, longToRational}

  val r: Rational = this

  override val approximations: Seq[Real.Approximation] = {
    val bound = Real.Approximation.RationalBound(this)
    Seq(Real.Approximation.Interval(bound, bound))
  }

  val sign: Int = numerator.signum

  lazy val isInt: Boolean = denominator == BigInt(1)

  lazy val integerPart: BigInt = numerator / denominator

  lazy val fractionalPart: Rational =
    new Rational(numerator % denominator, denominator)

  // noinspection ReferenceMustBePrefixed
  def toPositional(radix: Int = 10): String = {
    checkNumberFormatRadix(radix)

    @tailrec
    def formatFractionalPart(
        result: StringBuilder,
        remainders: LinkedHashSet[BigInt],
        remainder: BigInt,
    ): String = {
      if (remainder == BigInt(0)) {
        result.toString()
      }
      else if (remainders contains remainder) {
        val remainderIndex = remainders.iterator.indexOf(remainder)
        result.insert(remainderIndex, '(')
        result.append(')')
        result.toString()
      }
      else {
        remainders add remainder
        val quotient: BigInt = remainder / denominator
        if (!quotient.isValidInt || quotient >= radix) {
          // $COVERAGE-OFF$
          throw new RuntimeException(
            s"Bug in $getClass:" +
              s" failed to serialize $numerator/$denominator with radix = $radix:" +
              s" quotient is out of range: $quotient"
          )
          // $COVERAGE-ON$
        }
        result.append(Character.forDigit(quotient.toInt, radix))
        val newRemainder = remainder % denominator * radix
        formatFractionalPart(result, remainders, newRemainder)
      }
    }

    val integerPartResult = integerPart.toString(radix)
    if (this.isInt) {
      integerPartResult
    }
    else {
      val initialRemainder = fractionalPart.numerator.abs * radix
      val fractionalPartResult = formatFractionalPart(
        new StringBuilder(),
        LinkedHashSet(),
        initialRemainder,
      )
      s"$integerPartResult.$fractionalPartResult"
    }
  }

  def toCommonFraction(radix: Int = 10): String = {
    checkNumberFormatRadix(radix)
    val numeratorPart = numerator.toString(radix)
    if (isInt) {
      numeratorPart
    }
    else {
      s"$numeratorPart/${denominator.toString(radix)}"
    }
  }

  def toMixedFraction(radix: Int = 10): String = {
    checkNumberFormatRadix(radix)
    if (this.isInt) {
      numerator.toString(radix)
    }
    else if (integerPart == BigInt(0)) {
      toCommonFraction(radix)
    }
    else {
      s"${integerPart.toString(radix)} ${fractionalPart.abs.toCommonFraction(radix)}"
    }
  }

  override lazy val toString: String = toMixedFraction()

  override def unary_- : Rational = new Rational(-numerator, denominator)

  override def compare(that: Rational): Int = {
    val signumCompared = this.numerator.signum - that.numerator.signum
    if (signumCompared != 0) {
      signumCompared
    }
    else {
      (this.numerator * that.denominator) compare (that.numerator * this.denominator)
    }
  }

  override def abs: Rational = if (numerator < 0) -this else this

  def +(that: Rational): Rational = {
    Rational(
      this.numerator * that.denominator + that.numerator * this.denominator,
      this.denominator * that.denominator,
    )
  }

  override def +(that: Real): Real = that match {
    case r: Rational => this + r
    case _ => super.+(that)
  }

  def -(that: Rational): Rational = {
    this + -that
  }

  override def -(that: Real): Real = that match {
    case r: Rational => this - r
    case _ => super.-(that)
  }

  def *(that: Rational): Rational = {
    Rational(
      this.numerator * that.numerator,
      this.denominator * that.denominator,
    )
  }

  override def *(that: Real): Real = that match {
    case r: Rational => this * r
    case _ => super.*(that)
  }

  override def inverted: Rational = Rational(denominator, numerator)

  def /(that: Rational): Rational = {
    if (that.numerator == BigInt(0)) {
      throw new ArithmeticException("Division by zero")
    }
    this * that.inverted
  }

  override def /(that: Real): Real = that match {
    case r: Rational => this / r
    case _ => super./(that)
  }

  def floor: BigInt = {
    if (this.isInt || this > 0) {
      integerPart
    }
    else {
      integerPart - 1
    }
  }

  def ceil: BigInt = {
    if (this.isInt || this < 0) {
      integerPart
    }
    else {
      integerPart + 1
    }
  }

  override def round: BigInt = {
    val floor = this.floor
    val ceil = this.ceil
    val toFloor = this - floor
    val toCeil = ceil - this
    if (toFloor < toCeil) {
      floor
    }
    else if (toCeil < toFloor) {
      ceil
    }
    else if (this > 0) {
      ceil
    }
    else {
      floor
    }
  }

  override def round(precision: Rational): Rational = {
    require(precision > 0, s"Non-positive precision: $precision")
    (this / precision).round * precision
  }

  override def pow(exp: BigInt): Rational = {
    if (exp == BigInt(0)) {
      1
    }
    else if (exp < 0) {
      this.inverted pow -exp
    }
    else if (exp == BigInt(1) || this == Rational(0) || this == Rational(1)) {
      this
    }
    else if (this == Rational(-1)) {
      if (exp % 2 == BigInt(0)) {
        Rational(1)
      }
      else {
        this
      }
    }
    else {
      val intExp = exp.toInt
      new Rational(numerator pow intExp, denominator pow intExp)
    }
  }

  override def ^(exp: BigInt): Rational = this pow exp
}

object Rational {
  def apply(numerator: BigInt, denominator: BigInt): Rational = {
    if (denominator == 0) {
      throw new ArithmeticException("Division by zero")
    }
    else if (denominator < 0) {
      apply(-numerator, -denominator)
    }
    else {
      val gcd = denominator.gcd(numerator)
      new Rational(numerator / gcd, denominator / gcd)
    }
  }

  def apply(n: BigInt): Rational = new Rational(n, 1)

  private val intPattern = Pattern.compile("^-?[0-9a-zA-Z]+$")

  private val commonFractionPattern =
    Pattern.compile("""^(?<num>-?[0-9a-zA-Z]+)/(?<den>[0-9a-zA-Z]+)$""")

  private val mixedFractionPattern = Pattern.compile(
    """^(?<int>-?[0-9a-zA-Z]+) (?<num>[0-9a-zA-Z]+)/(?<den>[0-9a-zA-Z]+)$"""
  )

  private val positionalTerminatingPattern =
    Pattern.compile("""^(?<int>-?[0-9a-zA-Z]+)\.(?<frac>[0-9a-zA-Z]+)$""")

  private val positionalRepeatingPattern = Pattern.compile(
    """^(?<int>-?[0-9a-zA-Z]+)\.(?<nonRepeating>[0-9a-zA-Z]*)?\((?<repetend>[0-9a-zA-Z]+)\)$"""
  )

  @throws[NumberFormatException]
  def apply(serialized: String, radix: Int = 10): Rational = {
    checkNumberFormatRadix(radix)

    intPattern
      .matcher(serialized)
      .ifMatches { _ =>
        Rational(BigInt(serialized, radix))
      }
      .orElse(commonFractionPattern.matcher(serialized) ifMatches { matcher =>
        Rational(
          BigInt(matcher.group("num"), radix),
          BigInt(matcher.group("den"), radix),
        )
      })
      .orElse(mixedFractionPattern.matcher(serialized) ifMatches { matcher =>
        val intPartSource = matcher.group("int")
        val intPart = Rational(BigInt(intPartSource, radix))
        val fracPartAbs = Rational(
          BigInt(matcher.group("num"), radix),
          BigInt(matcher.group("den"), radix),
        )
        if (intPartSource startsWith "-") {
          intPart - fracPartAbs
        }
        else {
          intPart + fracPartAbs
        }
      })
      .orElse(positionalTerminatingPattern.matcher(serialized) ifMatches {
        matcher =>
          val intPartSource = matcher.group("int")
          val intPart = Rational(BigInt(intPartSource, radix))
          val normalizedFrac = matcher.group("frac") dropRightWhile { _ == '0' }
          if (normalizedFrac.isEmpty) {
            intPart
          }
          else {
            val numerator = BigInt(normalizedFrac, radix)
            val denominator = BigInt(radix) pow normalizedFrac.length
            val fracPartAbs = Rational(numerator, denominator)
            if (intPartSource startsWith "-") {
              intPart - fracPartAbs
            }
            else {
              intPart + fracPartAbs
            }
          }
      })
      .orElse(positionalRepeatingPattern.matcher(serialized) ifMatches {
        matcher =>
          val intPartSource = matcher.group("int")
          val intPart = Rational(BigInt(intPartSource, radix))
          val nonRepeating = matcher.group("nonRepeating")
          val repetend = matcher.group("repetend")
          val (nonRepeatingNumerator, nonRepeatingDenominator) = {
            if (nonRepeating.isEmpty) {
              (BigInt(0), BigInt(1))
            }
            else {
              (
                BigInt(nonRepeating, radix),
                BigInt(radix) pow nonRepeating.length,
              )
            }
          }
          val fracPartAbs =
            (
              Rational(nonRepeatingNumerator)
                + Rational(
                  BigInt(repetend, radix),
                  (BigInt(radix) pow repetend.length) - 1,
                )
            ) / Rational(nonRepeatingDenominator)
          if (intPartSource startsWith "-") {
            intPart - fracPartAbs
          }
          else {
            intPart + fracPartAbs
          }
      })
      .applyOrElse[Unit, Rational](
        (),
        _ =>
          throw new NumberFormatException(
            s"Cannot parse rational number: $serialized"
          ),
      )
  }
}
