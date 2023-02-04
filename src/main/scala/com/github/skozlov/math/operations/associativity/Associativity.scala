package com.github.skozlov.math.operations.associativity

trait Associativity[A, Op <: (A, A) => A] extends LeftAssociativity[A, A, Op, Op] with RightAssociativity[A, A, Op, Op]
