package com.github.skozlov.math.algebras

import com.github.skozlov.math.operations.distributivity.Distributivity
import com.github.skozlov.math.operations.Commutativity
import com.github.skozlov.math.operations.associativity.Associativity

trait Ring[A, Add <: (A, A) => A, Mul <: (A, A) => A]{
    def additionGroup: Group[A, Add]
    def additionCommutativity: Commutativity[A, A, Add]
    def multiplicationAssociativity: Associativity[A, Mul]
    def distributivity: Distributivity[A, Add, Mul]
}

