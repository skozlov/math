package com.github.skozlov.math.operations.distributivity

trait LeftDistributivity[A, B, Add <: (B, B) => B, Mul <: (A, B) => B]
