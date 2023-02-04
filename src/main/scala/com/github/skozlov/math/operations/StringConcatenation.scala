package com.github.skozlov.math.operations

object StringConcatenation extends ((String, String) => String){
    override def apply(a: String, b: String): String = a + b
}
