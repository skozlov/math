package com.github.skozlov.math.operations.id

trait RightId[A, B, Op <: (A, B) => A]{
    def rightId: B
}
