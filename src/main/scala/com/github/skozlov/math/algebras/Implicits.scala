package com.github.skozlov.math.algebras

import com.github.skozlov.math.operations.associativity.Associativity
import com.github.skozlov.math.operations.distributivity.Distributivity
import com.github.skozlov.math.operations.Commutativity
import com.github.skozlov.math.operations.id.Id
import com.github.skozlov.math.operations.inversion.{FullInversion, NonZeroInversion}

trait Implicits extends com.github.skozlov.math.operations.Implicits{
    implicit def monoidFromProperties[A, Op <: (A, A) => A]
    (implicit associativity: Associativity[A, Op], id: Id[A, Op]): Monoid[A, Op] = Monoid(associativity, id)

    implicit def groupFromProperties[A, Op <: (A, A) => A]
    (implicit monoid: Monoid[A, Op], inversion: FullInversion[A, Op]): Group[A, Op] = Group(monoid, inversion)

    implicit def ringFromProperties[A, Add <: (A, A) => A, Mul <: (A, A) => A]
    (implicit
        group: Group[A, Add],
        additionCommutativity: Commutativity[A, A, Add],
        multiplicationAssociativity: Associativity[A, Mul],
        distributivity: Distributivity[A, Add, Mul],
    ): Ring[A, Add, Mul] = {
        val _group = group
        val _additionCommutativity = additionCommutativity
        val _multiplicationAssociativity = multiplicationAssociativity
        val _distributivity = distributivity
        new Ring[A, Add, Mul] {
            override val additionGroup: Group[A, Add] = _group

            override val additionCommutativity: Commutativity[A, A, Add] = _additionCommutativity

            override val multiplicationAssociativity: Associativity[A, Mul] = _multiplicationAssociativity

            override val distributivity: Distributivity[A, Add, Mul] = _distributivity
        }
    }

    implicit def fieldFromProperties[A, Add <: (A, A) => A, Mul <: (A, A) => A]
    (implicit
        ring: Ring[A, Add, Mul],
        multiplicationCommutativity: Commutativity[A, A, Mul],
        multiplicationId: Id[A, Mul],
        multiplicationInversion: NonZeroInversion[A, Mul, Add],
    ): Field[A, Add, Mul] = {
        val _multiplicationCommutativity = multiplicationCommutativity
        val _multiplicationId = multiplicationId
        val _multiplicationInversion = multiplicationInversion
        new Field[A, Add, Mul] {
            override val additionGroup: Group[A, Add] = ring.additionGroup

            override val additionCommutativity: Commutativity[A, A, Add] = ring.additionCommutativity

            override val multiplicationCommutativity: Commutativity[A, A, Mul] = _multiplicationCommutativity

            override val multiplicationAssociativity: Associativity[A, Mul] = ring.multiplicationAssociativity

            override val multiplicationId: Id[A, Mul] = _multiplicationId

            override val multiplicationInversion: NonZeroInversion[A, Mul, Add] = _multiplicationInversion

            override val distributivity: Distributivity[A, Add, Mul] = ring.distributivity
        }
    }
}

object Implicits extends Implicits
