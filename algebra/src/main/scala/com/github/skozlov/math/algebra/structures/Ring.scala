package com.github.skozlov.math.algebra.structures

import com.github.skozlov.math.algebra.operation_properties.Commutativity
import com.github.skozlov.math.algebra.operation_properties.associativity.Associativity
import com.github.skozlov.math.algebra.operation_properties.distributivity.Distributivity

trait Ring[A, Add <: (A, A) => A, Mul <: (A, A) => A] {
  def additionGroup: Group[A, Add]
  def additionCommutativity: Commutativity[A, A, Add]
  def multiplicationAssociativity: Associativity[A, Mul]
  def distributivity: Distributivity[A, Add, Mul]
}
