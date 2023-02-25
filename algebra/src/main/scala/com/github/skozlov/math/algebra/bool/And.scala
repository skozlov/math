package com.github.skozlov.math.algebra.bool

object And extends ((Boolean, Boolean) => Boolean) {
  override def apply(a: Boolean, b: Boolean): Boolean = a && b
}
