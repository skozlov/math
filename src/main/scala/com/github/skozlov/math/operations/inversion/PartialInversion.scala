package com.github.skozlov.math.operations.inversion

trait PartialInversion[A, Op <: (A, A) => A]{
    def invertPartial: PartialFunction[A, A]
}
