package com.github.skozlov.math.algebra.bool

import com.github.skozlov.math.algebra.operation_properties.Commutativity
import com.github.skozlov.math.algebra.operation_properties.associativity.Associativity
import com.github.skozlov.math.algebra.operation_properties.distributivity.Distributivity
import com.github.skozlov.math.algebra.operation_properties.id.Id
import com.github.skozlov.math.algebra.operation_properties.inversion.{
  FullInversion,
  NonZeroInversion,
}

trait Implicits {
  implicit val orCommutativity: Commutativity[Boolean, Boolean, Or.type] =
    new Commutativity[Boolean, Boolean, Or.type] {}

  implicit val orAssociativity: Associativity[Boolean, Or.type] =
    new Associativity[Boolean, Or.type] {}

  implicit val orId: Id[Boolean, Or.type] = Id(false)

  implicit val xorCommutativity: Commutativity[Boolean, Boolean, Xor.type] =
    new Commutativity[Boolean, Boolean, Xor.type] {}

  implicit val xorAssociativity: Associativity[Boolean, Xor.type] =
    new Associativity[Boolean, Xor.type] {}

  implicit val xorId: Id[Boolean, Xor.type] = Id(false)

  implicit val xorInversion: FullInversion[Boolean, Xor.type] = b => b

  implicit val andCommutativity: Commutativity[Boolean, Boolean, And.type] =
    new Commutativity[Boolean, Boolean, And.type] {}

  implicit val andAssociativity: Associativity[Boolean, And.type] =
    new Associativity[Boolean, And.type] {}

  implicit val andId: Id[Boolean, And.type] = Id(true)

  implicit val andNonZeroXorInversion
      : NonZeroInversion[Boolean, And.type, Xor.type] =
    NonZeroInversion(_ => true)

  implicit val orAndDistributivity: Distributivity[Boolean, Or.type, And.type] =
    new Distributivity[Boolean, Or.type, And.type] {}

  implicit val andOrDistributivity: Distributivity[Boolean, And.type, Or.type] =
    new Distributivity[Boolean, And.type, Or.type] {}

  implicit val xorAndDistributivity
      : Distributivity[Boolean, Xor.type, And.type] =
    new Distributivity[Boolean, Xor.type, And.type] {}
}

object Implicits extends Implicits
