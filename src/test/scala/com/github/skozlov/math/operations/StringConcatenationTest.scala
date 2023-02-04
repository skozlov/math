package com.github.skozlov.math.operations

import Implicits._

class StringConcatenationTest extends Test {
    "String concatenation" should "be associative" in {
        checkAssociativity[String, StringConcatenation.type](StringConcatenation)("a", "b")
    }

    "Empty string" should "be an identity element" in {
        checkId[String, StringConcatenation.type](StringConcatenation)("abc")
    }
}
