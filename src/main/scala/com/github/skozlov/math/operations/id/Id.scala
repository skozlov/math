package com.github.skozlov.math.operations.id

trait Id[A, Op <: (A, A) => A] extends Object with LeftId[A, A, Op] with RightId[A, A, Op]{
    def id: A

    override def leftId: A = id

    override def rightId: A = id
}

object Id{
    def apply[A, Op <: (A, A) => A](id: A): Id[A, Op] = {
        val _id = id
        new Id[A, Op] {
            override val id: A = _id
        }
    }
}
