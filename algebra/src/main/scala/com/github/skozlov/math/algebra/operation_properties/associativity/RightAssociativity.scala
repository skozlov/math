package com.github.skozlov.math.algebra.operation_properties.associativity

/**
 * ABB(a1,ABB(a2,b)) = ABB(AAA(a1,a2),b)
 */
trait RightAssociativity[A, B, ABB <: (A, B) => B, AAA <: (A, A) => A]
