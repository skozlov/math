package com.github.skozlov.math.algebra.operation_properties.id

trait RightId[A, B, Op <: (A, B) => A] {
  def rightId: B
}
