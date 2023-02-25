package com.github.skozlov.math.algebra.operation_properties.inversion

trait PartialInversion[A, Op <: (A, A) => A] {
  def invertPartial: PartialFunction[A, A]
}
