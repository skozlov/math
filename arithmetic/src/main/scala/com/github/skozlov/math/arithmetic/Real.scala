package com.github.skozlov.math.arithmetic

import com.github.skozlov.commons.scala.lang
import Implicits.{longToRational, rationalToRealApproximationBound}
import com.github.skozlov.math.arithmetic.Real.Approximation.CheckedIterator

/**
 * Real number represented as a sequence of nested intervals
 * so that it can be calculated with arbitrary precision.
 *
 * @see [[https://en.wikipedia.org/wiki/Real_number]]
 * @see [[https://en.wikipedia.org/wiki/Nested_intervals]]
 * @see [[com.github.skozlov.math.arithmetic.Real#approximations()]]
 */
trait Real {
  import Real._

  /**
   * Sequence of nested intervals
   * each of which contains the number represented by `this`.
   *
   * The sequence should either be infinite
   * or end with the interval consisting of a single point
   * (the latter means that `this` is rational).
   *
   * The sequence may start with one or more NaNs.
   * NaN means that the interval is not defined yet.
   * NaN cannot follow a non-NaN interval.
   *
   * During the iteration over approximations
   * [[ArithmeticException]] may be thrown
   * if an interval does not contain the next one,
   * or if an interval is followed by NaN,
   * or if an approximation cannot be calculated
   * due to an implementation-specific reason,
   *
   * @see [[https://en.wikipedia.org/wiki/Nested_intervals]]
   * @see [[https://en.wikipedia.org/wiki/Rational_number]]
   * @see [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]]
   */
  def approximations: Seq[Approximation]

  /**
   * Returns `Real` which represents the same number
   * but whose `toString` returns the specified string.
   */
  def withStringRepresentation(stringRepresentation: String): Real = {
    if (stringRepresentation == toString) {
      this
    }
    else {
      val self = this
      new Real {
        override def approximations: Seq[Approximation] = self.approximations

        override val toString: String = stringRepresentation
      }
    }
  }

  private def map(
      stringRepresentation: String
  )(f: Approximation => Approximation): Real = {
    Real(stringRepresentation, approximations map f)
  }

  private def zip(that: Real, stringRepresentation: String)(
      f: (Approximation, Approximation) => Approximation
  ): Real = {
    val self = this
    Real(
      stringRepresentation,
      new IterableOnce[Approximation] {
        override def iterator: Iterator[Approximation] =
          new Iterator[Approximation] {
            private val iterators = Array(self, that) map {
              _.approximations.iterator
            }
            private val lastValues = new Array[Approximation](2)

            override def hasNext: Boolean = iterators exists {
              _.hasNext
            }

            override def next(): Approximation = {
              if (!hasNext) {
                // $COVERAGE-OFF$
                throw new NoSuchElementException("Last approximation reached")
                // $COVERAGE-ON$
              }
              for (i <- iterators.indices) {
                val it = iterators(i)
                if (it.hasNext) {
                  lastValues(i) = it.next()
                }
              }
              f(lastValues(0), lastValues(1))
            }
          }
      },
    )
  }

  /**
   * Nearest `n * precision` where `n` is an integer. If `this` is right in the
   * middle of two such numbers then the one with the greater absolute value is
   * returned.
   *
   * @example
   * {{{π round 1.r / 100000 // 314159.r / 100000}}}
   */
  def round(precision: Rational): Rational = {
    require(precision > 0, s"Non-positive precision: $precision")
    approximations.iterator
      .flatMap { _ round precision }
      .next()
  }

  /**
   * Nearest integer to this number. If this number is right in the middle of
   * two integers (i.e. it's fractional part is 1/2) then the one with the
   * greater absolute value is returned.
   *
   * @example
   * {{{π.round // 3}}}
   * @example
   * {{{(π + (1.r/2)).round // 4}}}
   * @example
   * {{{(-3.r / 2).round // -2}}}
   */
  def round: BigInt = round(1.r).numerator

  def unary_- : Real = map(s"-($toString)") { -_ }

  /**
   * Absolute value of this number.
   *
   * @see
   *   [[https://en.wikipedia.org/wiki/Absolute_value]]
   */
  def abs: Real = map(s"|($toString)|") { _.abs }

  def +(that: Real): Real =
    zip(that, s"(${this.toString}) + (${that.toString})") { _ + _ }

  def -(that: Real): Real =
    zip(that, s"(${this.toString}) - (${that.toString})") { _ - _ }

  def *(that: Real): Real =
    zip(that, s"(${this.toString}) * (${that.toString})") { _ * _ }

  /**
   * 1 / this.
   *
   * @throws ArithmeticException
   *   if this number is 0.
   *   This exception may also be thrown while iterating through result approximations.
   */
  def inverted: Real = map(s"1/($this)") { _.inverted }

  /**
   * @throws ArithmeticException
   *   if `that` is 0.
   *   This exception may also be thrown while iterating through result approximations.
   */
  def /(that: Real): Real =
    this * that.inverted withStringRepresentation s"(${this.toString}) / (${that.toString})"

  /**
   * This number raised to the power of `exp`.
   *
   * @throws ArithmeticException
   *   if this number is 0 and `exp` is negative.
   *   This exception may also be thrown while iterating through result approximations.
   */
  def pow(exp: BigInt): Real = {
    if (exp == BigInt(0)) {
      1
    }
    else if (exp == BigInt(1)) {
      this
    }
    else {
      map(s"($this) ^ $exp") { _ pow exp }
    }
  }

  /**
   * Alias for [[com.github.skozlov.math.arithmetic.Real#pow(scala.math.BigInt)]].
   */
  def ^(exp: BigInt): Real = this pow exp
}

object Real {

  /**
   * Either a number interval
   * or [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]]
   * (which means that interval is not defined).
   */
  sealed trait Approximation {

    /**
     * @return
     *   `true` if
     *   this is [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]]
     *   or any point from `that` also belongs to `this`,
     *   `false` otherwise.
     */
    def includes(that: Approximation): Boolean

    /**
     * @return
     *   `Some(r)` if `this` is an interval for any point `p` of which
     *   `p round precision == r`,
     *   `None` otherwise.
     */
    def round(precision: Rational): Option[Rational]

    /**
     * @return
     * Interval consisting of negations (`-p`) of all points from this interval.
     * [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]] if this is NaN.
     */
    def unary_- : Approximation

    /**
     * @return
     * Interval consisting of absolute values of all points from this interval.
     * [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]] if this is NaN.
     */
    def abs: Approximation

    /**
     * @return
     * Interval consisting of all sums like `a + b`
     * where `a` is a point from `this` interval
     * and `b` is a point from `that` interval.
     * [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]]
     * if `this` or `that` is NaN.
     */
    def +(that: Approximation): Approximation

    /**
     * @return
     * Interval consisting of all differences like `a - b`
     * where `a` is a point from `this` interval
     * and `b` is a point from `that` interval.
     * [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]]
     * if `this` or `that` is NaN.
     */
    def -(that: Approximation): Approximation = this + -that

    /**
     * @return
     * Interval consisting of all products like `a * b`
     * where `a` is a point from `this` interval
     * and `b` is a point from `that` interval.
     * [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]]
     * if `this` or `that` is NaN.
     */
    def *(that: Approximation): Approximation

    /**
     * @return
     * Interval consisting of intertions (`1/p`) of all points from this interval.
     * [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]]
     * if this is NaN or an interval which contains `0`.
     * @throws ArithmeticException
     * if `this` is an interval which contains only `0`.
     */
    def inverted: Approximation

    /**
     * @return
     * Interval consisting of all powers like `p pow exp`
     * where `a` is a point from `this` interval.
     * [[com.github.skozlov.math.arithmetic.Real.Approximation.Nan]]
     * if `this` is NaN or if `exp < 0` an `this` is an interval which contains `0`.
     * @throws ArithmeticException
     * if `exp < 0` and `this` is an interval which contains only `0`.
     * @see [[com.github.skozlov.math.arithmetic.Rational#pow(scala.math.BigInt)]]
     */
    def pow(exp: BigInt): Approximation
  }

  object Approximation {

    /**
     * Either minimal or maximal value of an approximation interval.
     * It's either a rational number or positive/negative infinity.
     * @see [[com.github.skozlov.math.arithmetic.Real.Approximation.Interval]]
     */
    sealed trait Bound extends Ordered[Bound] {

      /**
       * @return
       * `true` if `this` is positive or negative infinity,
       * `false` if `this` is a number.
       */
      def isFinite: Boolean

      /**
       * @return
       * `0` if `this` is `0`,
       * `1` if `this` is positive infinity or a positive number,
       * `-1` if `this` is negativbe infinity or a negative number.
       */
      def sign: Int

      /**
       * @return
       * `-n` if `this` is a rational number,
       * `-∞` for `+∞`,
       * `+∞` for `-∞`.
       * @see [[com.github.skozlov.math.arithmetic.Rational#unary_-]]
       */
      def unary_- : Bound

      /**
       * @return
       * Absolute value of this bound,
       * `+∞` if it is not finite.
       */
      def abs: Bound

      /**
       * Multiplication of `this` and `that`.
       * If both `this` and `that` are numbers then the result is a number
       * calculated by [[com.github.skozlov.math.arithmetic.Rational#*(com.github.skozlov.math.arithmetic.Rational)]].
       * If `this` or `that` is infinite then the result depends on their signs:
       * <ul>
       *     <li>`0` if they are both `0`.</li>
       *     <li>`-∞` if one of them is positive and another is negative.</li>
       *     <li>`+∞` if they are both positive or negative.</li>
       * </ul>
       */
      def *(that: Bound): Bound

      def pow(exp: BigInt): Bound
    }

    case object PositiveInfinity extends Bound {
      override val toString: String = "+∞"

      override val isFinite: Boolean = false

      override val sign: Int = 1

      override def compare(that: Bound): Int = if (this == that) 0 else 1

      override val unary_- : NegativeInfinity.type = NegativeInfinity

      override val abs: PositiveInfinity.type = PositiveInfinity

      override def *(that: Bound): Bound = that.sign match {
        case 0 => that
        case 1 => +∞
        case -1 => -∞
      }

      override def pow(exp: BigInt): Bound = exp.signum match {
        case 0 => RationalBound(1)
        case 1 => this
        case -1 => RationalBound(0)
      }
    }

    case object NegativeInfinity extends Bound {
      override val toString: String = "-∞"

      override val isFinite: Boolean = false

      override val sign: Int = -1

      override def compare(that: Bound): Int = if (this == that) 0 else -1

      override val unary_- : PositiveInfinity.type = PositiveInfinity

      override val abs: PositiveInfinity.type = PositiveInfinity

      override def *(that: Bound): Bound = that.sign match {
        case 0 => that
        case 1 => -∞
        case -1 => +∞
      }

      override def pow(exp: BigInt): Bound = exp.signum match {
        case 0 => RationalBound(1)
        case -1 => RationalBound(0)
        case 1 => if (exp % 2 == BigInt(0)) PositiveInfinity else this
      }
    }

    val +∞ : PositiveInfinity.type = PositiveInfinity
    val -∞ : NegativeInfinity.type = NegativeInfinity

    case class RationalBound(r: Rational) extends Bound {
      override def toString: String = r.toString

      override val isFinite: Boolean = true

      override def sign: Int = r.sign

      override def compare(b: Bound): Int = b match {
        case that: RationalBound => this.r compare that.r
        case _ => -(b compare this)
      }

      override def unary_- : RationalBound = RationalBound(-r)

      override def abs: RationalBound = RationalBound(r.abs)

      override def *(b: Bound): Bound = b match {
        case RationalBound(that) => this.r * that.r
        case _ => b * this
      }

      override def pow(exp: BigInt): RationalBound = RationalBound(r pow exp)
    }

    case object Nan extends Approximation {
      override val toString: String = "NaN"

      override def includes(that: Approximation): Boolean = true

      override def round(precision: Rational): Option[Rational] = None

      override def unary_- : Nan.type = Nan

      override val abs: Nan.type = Nan

      override def +(that: Approximation): Nan.type = Nan

      override def *(that: Approximation): Nan.type = Nan

      override def inverted: Nan.type = Nan

      override def pow(exp: BigInt): Nan.type = Nan
    }

    case class Interval private (min: Bound, max: Bound) extends Approximation {
      override def toString: String = {
        val leftBracket = if (min.isFinite) '[' else '('
        val rightBracket = if (max.isFinite) ']' else ')'
        s"$leftBracket$min, $max$rightBracket"
      }

      override def includes(a: Approximation): Boolean = a match {
        case Nan => false
        case that: Interval => this.min <= that.min && that.max <= this.max
      }

      override def round(precision: Rational): Option[Rational] = this match {
        case Interval(RationalBound(min), RationalBound(max)) =>
          val minRound = min round precision
          val maxRound = max round precision
          if (minRound == maxRound) Some(minRound) else None
        case _ => None
      }

      override def unary_- : Interval = new Interval(-max, -min)

      override def abs: Interval = {
        if (min.sign >= 0) {
          this
        }
        else if (max.sign <= 0) {
          -this
        }
        else {
          Interval(0.r, lang.max(min.abs, max.abs))
        }
      }

      override def +(a: Approximation): Approximation = a match {
        case Nan => Nan
        case that: Interval =>
          if (
            Seq((this.min, that.max), (that.min, this.max)) contains ((-∞, +∞))
          ) {
            Interval(-∞, +∞)
          }
          else {
            def sum(a: Bound, b: Bound): Bound = (a, b) match {
              case (RationalBound(x), RationalBound(y)) => RationalBound(x + y)
              case _ => Seq(a, b).find { !_.isFinite }.get
            }

            Interval(sum(this.min, that.min), sum(this.max, that.max))
          }
      }

      override def *(a: Approximation): Approximation = a match {
        case Nan => Nan
        case that: Interval =>
          val intervals = Seq(this, that) sortBy { i =>
            (i.min.sign, i.max.sign)
          }
          val left = intervals.head
          val right = intervals(1)
          if (left.min.sign >= 0) {
            Interval(left.min * right.min, left.max * right.max)
          }
          else if (right.max.sign < 0) {
            Interval(left.max * right.max, left.min * right.min)
          }
          else if (left.max.sign < 0) {
            if (right.min.sign >= 0) {
              Interval(left.min * right.max, left.max * right.min)
            }
            else {
              Interval(left.min * right.max, left.min * right.min)
            }
          }
          else if (right.min.sign >= 0) {
            Interval(left.min * right.max, left.max * right.max)
          }
          else {
            Interval(
              lang.min(left.min * right.max, left.max * right.min),
              lang.max(left.min * right.min, left.max * right.max),
            )
          }
      }

      override def inverted: Approximation = {
        if (min.sign <= 0 && max.sign >= 0) {
          if (min.sign == max.sign) {
            throw new ArithmeticException("Division by zero")
          }
          Nan
        }
        else {
          def inverse(bound: Bound): RationalBound = bound match {
            case PositiveInfinity | NegativeInfinity => 0.r
            case RationalBound(r) => 1 / r
          }

          new Interval(inverse(max), inverse(min))
        }
      }

      override def pow(exp: BigInt): Approximation = {
        if (exp < 0) {
          this.inverted pow -exp
        }
        else if (min.sign < 0 && max.sign > 0 && exp % 2 == BigInt(0)) {
          Interval(0.r pow exp, lang.max(min.abs, max.abs) pow exp)
        }
        else {
          Interval(min pow exp, max pow exp)
        }
      }
    }

    object Interval {
      def apply(a: Bound, b: Bound): Interval = {
        require(
          a != b || a.isFinite,
          s"Invalid approximation interval: [$a, $a]",
        )
        if (a > b) {
          new Interval(b, a)
        }
        else {
          new Interval(a, b)
        }
      }
    }

    class CheckedIterator(val wrapped: Iterator[Approximation])
        extends Iterator[Approximation] {
      private var previous: Approximation = _

      override def hasNext: Boolean = wrapped.hasNext

      override def next(): Approximation = {
        val current = wrapped.next()
        if (previous != null && !(previous includes current)) {
          throw new ArithmeticException(
            s"Real number approximation $previous does not include the next approximation $current"
          )
        }
        if (!hasNext) {
          current match {
            case Interval(RationalBound(x), RationalBound(y)) if x == y =>
            case _ =>
              throw new ArithmeticException(
                s"Last approximation of the real number is not a single rational number but $current"
              )
          }
        }
        previous = current
        current
      }
    }

    object CheckedIterator {
      def apply(iterator: Iterator[Approximation]): CheckedIterator =
        iterator match {
          case it: CheckedIterator => it
          case _ => new CheckedIterator(iterator)
        }
    }
  }

  def apply(
      stringRepresentation: => String,
      approximations: IterableOnce[Approximation],
  ): Real = {
    val a = approximations
    new Real {
      override def toString: String = stringRepresentation

      override val approximations: Seq[Approximation] =
        LazyList.from(new Iterable[Approximation] {
          override def iterator: Iterator[Approximation] =
            CheckedIterator(a.iterator)
        })
    }
  }
}
