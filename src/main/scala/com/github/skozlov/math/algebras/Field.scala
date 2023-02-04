package com.github.skozlov.math.algebras

import com.github.skozlov.math.operations.Commutativity
import com.github.skozlov.math.operations.id.Id
import com.github.skozlov.math.operations.inversion.NonZeroInversion

trait Field[A, Add <: (A, A) => A, Mul <: (A, A) => A] extends Ring[A, Add, Mul]{
    def multiplicationCommutativity: Commutativity[A, A, Mul]
    def multiplicationId: Id[A, Mul]
    def multiplicationInversion: NonZeroInversion[A, Mul, Add]
}
