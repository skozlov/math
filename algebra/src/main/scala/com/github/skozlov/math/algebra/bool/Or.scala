package com.github.skozlov.math.algebra.bool

object Or extends ((Boolean, Boolean) => Boolean) {
  override def apply(a: Boolean, b: Boolean): Boolean = a || b
}
