package com.github.skozlov.math.arithmetic

import com.github.skozlov.commons.scala.lang.{
  RichMatcher,
  RichString,
  checkNumberFormatRadix,
}

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable.{LinkedHashSet, StringBuilder}

/**
 * Number which is represented as a fraction of two integers, a numerator and a
 * denominator.
 *
 * It is in the canonical form, i.e. the fraction is irreducible and the
 * denominator is positive.
 *
 * @see
 *   [[https://en.wikipedia.org/wiki/Rational_number]]
 */
case class Rational private (numerator: BigInt, denominator: BigInt)
    extends Real
    with Ordered[Rational] {

  import Implicits.{bigIntToRational, longToRational}

  /**
   * Helps to implicitly convert a value to Rational.
   *
   * @example
   *   {{{
   *   import com.github.skozlov.math.arithmetic.Implicits._
   *   val half = 1.r / 2
   *   }}}
   */
  val r: Rational = this

  /**
   * Sequence with one element, interval containing only this number itself.
   */
  override val approximations: Seq[Real.Approximation] = {
    val bound = Real.Approximation.RationalBound(this)
    Seq(Real.Approximation.Interval(bound, bound))
  }

  /**
   * -1 if this number is less than 0, +1 if it is greater than 0, 0 if it is
   * equal to 0.
   */
  val sign: Int = numerator.signum

  /**
   * true if this number is an integer number, false otherwise
   */
  lazy val isInt: Boolean = denominator == BigInt(1)

  /**
   * Nearest integer which is equal to this number or is closer to 0.
   *
   * @example
   *   {{{-2.integerPart // -2}}}
   * @example
   *   {{{(-5.r / 2).integerPart // -2}}}
   * @example
   *   {{{(5.r / 2).integerPart // 2}}}
   */
  lazy val integerPart: BigInt = numerator / denominator

  /**
   * This number minus its [[integerPart]].
   *
   * @example
   *   {{{(5.r / 2).fractionalPart // 1/2}}}
   * @example
   *   {{{(-5.r / 2).fractionalPart // -1/2}}}
   */
  lazy val fractionalPart: Rational =
    new Rational(numerator % denominator, denominator)

  /**
   * Represents this number in positional notation with the specified radix (10
   * by default). The repetend (if any) is enclosed in parentheses.
   * @throws IllegalArgumentException
   *   if radix is less than 2 or greater than 36
   * @example
   *   {{{(1.r / 2).toPositional() // 0.5}}}
   * @example
   *   {{{(1.r / 3).toPositional() // 0.(3)}}}
   * @see
   *   [[https://en.wikipedia.org/wiki/Repeating_decimal]]
   * @see
   *   [[https://en.wikipedia.org/wiki/Positional_notation]]
   */
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

  /**
   * Represents this number as a fraction of its numerator and denominator, both
   * represented in positional notation with the specified radix (10 by
   * default). If this number is an integer then the denominator is omitted.
   *
   * @throws IllegalArgumentException
   *   if radix is less than 2 or greater than 36
   * @example
   *   {{{3.toCommonFraction(2) // 11}}}
   * @example
   *   {{{(-4.r / 3).toCommonFraction(2) // -100/11}}}
   * @see
   *   [[https://en.wikipedia.org/wiki/Fraction]]
   * @see
   *   [[https://en.wikipedia.org/wiki/Positional_notation]]
   */
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

  /**
   * Represents this number as a sum of its quotient and remainder, both
   * represented in positional notation with the specified radix (10 by
   * default).
   *
   * If this number is an integer then its remainder (0) is omitted.
   *
   * If this number is not an integer and its quotient is 0 then this quotient
   * is omitted.
   *
   * @throws IllegalArgumentException
   *   if radix is less than 2 or greater than 36
   * @example
   *   {{{0.toMixedFraction(2) // 0}}}
   * @example
   *   {{{(-2).toMixedFraction(2) // -10}}}
   * @example
   *   {{{(-1.r / 2).toMixedFraction(2) // -1/10}}}
   * @example
   *   {{{(-3.r / 2).toMixedFraction(2) // -1 1/10}}}
   * @see
   *   [[https://en.wikipedia.org/wiki/Fraction]]
   * @see
   *   [[https://en.wikipedia.org/wiki/Positional_notation]]
   */
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

  /**
   * The same as [[toMixedFraction]] with radix = 10.
   */
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

  /**
   * 1 / this.
   * @throws ArithmeticException
   *   if this number is 0.
   */
  override def inverted: Rational = Rational(denominator, numerator)

  /**
   * @throws ArithmeticException
   *   if `that` is 0
   */
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

  /**
   * Largest integer which is less than or equal to this number.
   */
  def floor: BigInt = {
    if (this.isInt || this > 0) {
      integerPart
    }
    else {
      integerPart - 1
    }
  }

  /**
   * Smallest integer which is greater than or equal to this number.
   */
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

  /**
   * This number raised to the power of `exp`.
   *
   * @throws ArithmeticException
   *   if this number is 0 and `exp` is negative.
   */
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

  /**
   * Alias for [[com.github.skozlov.math.arithmetic.Rational#pow(scala.math.BigInt)]].
   */
  override def ^(exp: BigInt): Rational = this pow exp
}

object Rational {

  /**
   * Creates `Rational` equal to `numerator / denominator` (where `/` is the
   * standard division, not the integer division). Note that the fraction is
   * converted to its canonical form, i.e. the resulting fraction is irreducible
   * and the denominator is positive.
   *
   * @throws ArithmeticException
   *   if `denominator` is 0.
   */
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

  /**
   * Creates `Rational` representing the given integer. It is an equivalent of
   * `Rational(n, 1)`.
   */
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

  /**
   * Translates the string representation of a `Rational` in the specified `radix` into a `Rational`.
   *
   * Supported representations:
   * <ul>
   *     <li>
   *         Integer (optional `-` followed by non-empty sequence of alpha-numeric symbols), e.g. `-123`.
   *     </li>
   *     <li>
   *         Common fraction (`numerator`/`denominator`), e.g. `-123/456`.
   *     </li>
   *     <li>
   *         Mixed fraction (quotient and remainder separated by the whitespace), e.g. `-123 456/789`.
   *     </li>
   *     <li>
   *         Positional fraction with optional trailing repeating pattern, e.g. `-123.456` or `-123.456(789)`.
   *     </li>
   * </ul>
   *
   * @throws IllegalArgumentException if radix is less than 2 or greater than 36.
   * @throws NumberFormatException    if `serialized` is not a valid representation of `Rational` in the specified `radix`.
   */
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
