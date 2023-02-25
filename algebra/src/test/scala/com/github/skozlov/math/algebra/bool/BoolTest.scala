package com.github.skozlov.math.algebra.bool

import com.github.skozlov.math.algebra.Test

class BoolTest extends Test {
  "<{true,false}, {xor, and}>" should "be a field" in {
    checkField[Boolean, Xor.type, And.type](Xor, And)(true, false)
  }

  "<{true,false}, {or}>" should "be a commutative monoid" in {
    checkCommutativity[Boolean, Boolean, Or.type](Or)(true, false)
    checkMonoid[Boolean, Or.type](Or)(true, false)
  }

  "and and or" should "be distributive" in {
    checkDistributivity[Boolean, And.type, Or.type](And, Or)(true, false)
    checkDistributivity[Boolean, Or.type, And.type](Or, And)(true, false)
  }
}
