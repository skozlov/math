package com.github.skozlov.math.algebra

trait Implicits
    extends structures.Implicits
    with arithmetic.Implicits
    with bool.Implicits

object Implicits extends Implicits
