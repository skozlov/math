package com.github.skozlov.math.arithmetic

import com.github.skozlov.commons.scala.test.Test
import Real.Approximation.CheckedIterator

class RealApproximationCheckedIteratorTest extends Test {
  "apply(CheckedIterator)" should "not wrap CheckedIterator" in {
    val it = CheckedIterator(Iterator.empty)
    CheckedIterator(it) should be theSameInstanceAs it
  }
}
