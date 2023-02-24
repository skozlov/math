package com.github.skozlov.math.algebra.operation_properties.id

trait LeftId[A, B, Op <: (A, B) => B] {
    def leftId: A
}
