package com.github.skozlov.math.algebra.operation_properties.associativity

trait Associativity[A, Op <: (A, A) => A]
    extends LeftAssociativity[A, A, Op, Op]
    with RightAssociativity[A, A, Op, Op]
