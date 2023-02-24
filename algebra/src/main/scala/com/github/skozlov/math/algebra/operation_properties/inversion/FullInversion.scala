package com.github.skozlov.math.algebra.operation_properties.inversion

trait FullInversion[A, Op <: (A, A) => A] extends PartialInversion[A, Op]{
    def invert(a: A): A

    override val invertPartial: PartialFunction[A, A] = {case a => invert(a)}
}
