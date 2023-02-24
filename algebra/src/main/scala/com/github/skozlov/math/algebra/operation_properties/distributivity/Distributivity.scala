package com.github.skozlov.math.algebra.operation_properties.distributivity

trait Distributivity[A, Add <: (A, A) => A, Mul <: (A, A) => A] extends Object
    with LeftDistributivity[A, A, Add, Mul]
    with RightDistributivity[A, A, Add, Mul]
