package com.github.skozlov.math.algebra.operation_properties.associativity

/**
 * ABA(ABA(a,b1),b2) = ABA(a,BBB(b1,b2))
 */
trait LeftAssociativity[A, B, ABA <: (A, B) => A, BBB <: (B, B) => B]
