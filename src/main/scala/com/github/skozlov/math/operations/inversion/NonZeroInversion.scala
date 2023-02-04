package com.github.skozlov.math.operations.inversion

import com.github.skozlov.math.operations.id.Id

trait NonZeroInversion[A, Op <: (A, A) => A, ZeroOp <: (A, A) => A] extends PartialInversion[A, Op]

object NonZeroInversion{
    def apply[A, Op <: (A, A) => A, ZeroOp <: (A, A) => A](invert: A => A)(implicit zero: Id[A, ZeroOp])
    : NonZeroInversion[A, Op, ZeroOp] = {
        new NonZeroInversion[A, Op, ZeroOp] {
            override val invertPartial: PartialFunction[A, A] = {
                case a if a != zero.id => invert(a)
            }
        }
    }
}
