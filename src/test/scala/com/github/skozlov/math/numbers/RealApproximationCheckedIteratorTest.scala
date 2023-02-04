package com.github.skozlov.math.numbers

import com.github.skozlov.math.commons.test.Test
import com.github.skozlov.math.numbers.Real.Approximation.CheckedIterator

class RealApproximationCheckedIteratorTest extends Test {
    "apply(CheckedIterator)" should "not wrap CheckedIterator" in {
        val it = CheckedIterator(Iterator.empty)
        CheckedIterator(it) should be theSameInstanceAs it
    }
}
