package com.github.skozlov.math.operations.associativity

/**
 * ABA(ABA(a,b1),b2) = ABA(a,BBB(b1,b2))
 */
trait LeftAssociativity[A, B, ABA <: (A, B) => A, BBB <: (B, B) => B]
