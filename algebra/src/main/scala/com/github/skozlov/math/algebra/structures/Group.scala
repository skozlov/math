package com.github.skozlov.math.algebra.structures

import com.github.skozlov.math.algebra.operation_properties.associativity.Associativity
import com.github.skozlov.math.algebra.operation_properties.id.Id
import com.github.skozlov.math.algebra.operation_properties.inversion.FullInversion

trait Group[A, Op <: (A, A) => A] extends Monoid[A, Op] {
  def inversion: FullInversion[A, Op]
}

object Group {
  def apply[A, Op <: (A, A) => A](
      monoid: Monoid[A, Op],
      inversion: FullInversion[A, Op],
  ): Group[A, Op] = {
    val _inversion = inversion
    new Group[A, Op] {
      override val associativity: Associativity[A, Op] = monoid.associativity

      override val id: Id[A, Op] = monoid.id

      override val inversion: FullInversion[A, Op] = _inversion
    }
  }
}
