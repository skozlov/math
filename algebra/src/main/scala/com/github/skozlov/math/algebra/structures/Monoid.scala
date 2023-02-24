package com.github.skozlov.math.algebra.structures

import com.github.skozlov.math.algebra.operation_properties.associativity.Associativity
import com.github.skozlov.math.algebra.operation_properties.id.Id

trait Monoid[A, Op <: (A, A) => A]{
    def associativity: Associativity[A, Op]

    def id: Id[A, Op]
}

object Monoid{
    def apply[A, Op <: (A, A) => A](associativity: Associativity[A, Op], id: Id[A, Op]): Monoid[A, Op] = {
        val _associativity = associativity
        val _id = id
        new Monoid[A, Op] {
            override val associativity: Associativity[A, Op] = _associativity

            override val id: Id[A, Op] = _id
        }
    }
}
