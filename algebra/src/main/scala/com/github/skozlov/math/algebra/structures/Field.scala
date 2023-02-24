package com.github.skozlov.math.algebra.structures

import com.github.skozlov.math.algebra.operation_properties.Commutativity
import com.github.skozlov.math.algebra.operation_properties.id.Id
import com.github.skozlov.math.algebra.operation_properties.inversion.NonZeroInversion

trait Field[A, Add <: (A, A) => A, Mul <: (A, A) => A] extends Ring[A, Add, Mul]{
    def multiplicationCommutativity: Commutativity[A, A, Mul]
    def multiplicationId: Id[A, Mul]
    def multiplicationInversion: NonZeroInversion[A, Mul, Add]
}
