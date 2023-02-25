package com.github.skozlov.commons.scala

import java.util.regex.Matcher

package object lang {
  def min[A](a: A, other: A*)(implicit ordering: Ordering[A]): A =
    (a +: other).min

  def max[A](a: A, other: A*)(implicit ordering: Ordering[A]): A =
    (a +: other).max

  val NumberFormatValidRadixes: Range =
    Character.MIN_RADIX to Character.MAX_RADIX

  def checkNumberFormatRadix(radix: Int): Unit = {
    require(
      NumberFormatValidRadixes contains radix,
      s"Radix $radix is out of range $NumberFormatValidRadixes",
    )
  }

  implicit class RichMatcher(matcher: Matcher) {
    def ifMatches[R](f: Matcher => R): PartialFunction[Unit, R] = {
      case _ if matcher.matches() => f(matcher)
    }
  }

  implicit class RichString(s: String) {
    def dropRightWhile(f: Char => Boolean): String = {
      s.lastIndexWhere(f) match {
        case -1 => s
        case i => s take (i - 1)
      }
    }
  }
}
