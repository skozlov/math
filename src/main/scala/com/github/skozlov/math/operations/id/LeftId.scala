package com.github.skozlov.math.operations.id

trait LeftId[A, B, Op <: (A, B) => B] {
    def leftId: A
}
