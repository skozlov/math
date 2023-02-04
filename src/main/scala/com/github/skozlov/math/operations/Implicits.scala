package com.github.skozlov.math.operations

import com.github.skozlov.math.numbers.{Complex, Rational, Real}
import com.github.skozlov.math.operations.associativity.Associativity
import com.github.skozlov.math.operations.distributivity.Distributivity
import com.github.skozlov.math.operations.id.Id
import com.github.skozlov.math.operations.inversion.{FullInversion, NonZeroInversion}

trait Implicits extends com.github.skozlov.math.numbers.Implicits {
    implicit val intAdditionCommutativity: Commutativity[BigInt, BigInt, IntAddition.type] = {
        new Commutativity[BigInt, BigInt, IntAddition.type] {}
    }

    implicit val intAdditionAssociativity: Associativity[BigInt, IntAddition.type] = {
        new Associativity[BigInt, IntAddition.type] {}
    }

    implicit val intAdditionId: Id[BigInt, IntAddition.type] = Id(0)

    implicit val intAdditionInversion: FullInversion[BigInt, IntAddition.type] = -_

    implicit val intMultiplicationCommutativity: Commutativity[BigInt, BigInt, IntMultiplication.type] = {
        new Commutativity[BigInt, BigInt, IntMultiplication.type] {}
    }

    implicit val intMultiplicationAssociativity: Associativity[BigInt, IntMultiplication.type] = {
        new Associativity[BigInt, IntMultiplication.type] {}
    }

    implicit val intMultiplicationId: Id[BigInt, IntMultiplication.type] = Id(1)

    implicit val intDistributivity: Distributivity[BigInt, IntAddition.type, IntMultiplication.type] = {
        new Distributivity[BigInt, IntAddition.type, IntMultiplication.type]{}
    }

    implicit val rationalAdditionCommutativity: Commutativity[Rational, Rational, RationalAddition.type] = {
        new Commutativity[Rational, Rational, RationalAddition.type] {}
    }

    implicit val rationalAdditionAssociativity: Associativity[Rational, RationalAddition.type] = {
        new Associativity[Rational, RationalAddition.type]{}
    }

    implicit val rationalAdditionId: Id[Rational, RationalAddition.type] = Id(0)

    implicit val rationalAdditionInversion: FullInversion[Rational, RationalAddition.type] = -_

    implicit val rationalMultiplicationCommutativity: Commutativity[Rational, Rational, RationalMultiplication.type] = {
        new Commutativity[Rational, Rational, RationalMultiplication.type] {}
    }

    implicit val rationalMultiplicationAssociativity: Associativity[Rational, RationalMultiplication.type] = {
        new Associativity[Rational, RationalMultiplication.type]{}
    }

    implicit val rationalMultiplicationId: Id[Rational, RationalMultiplication.type] = Id(1)

    implicit val rationalMultiplicationInversion
    : NonZeroInversion[Rational, RationalMultiplication.type, RationalAddition.type] = NonZeroInversion(1/_)

    implicit val rationalDistributivity
    : Distributivity[Rational, RationalAddition.type, RationalMultiplication.type] = {
        new Distributivity[Rational, RationalAddition.type, RationalMultiplication.type]{}
    }

    implicit val realAdditionCommutativity: Commutativity[Real, Real, RealAddition.type] = {
        new Commutativity[Real, Real, RealAddition.type] {}
    }

    implicit val realAdditionAssociativity: Associativity[Real, RealAddition.type] = {
        new Associativity[Real, RealAddition.type]{}
    }

    implicit val realAdditionId: Id[Real, RealAddition.type] = Id(0)

    implicit val realAdditionInversion: FullInversion[Real, RealAddition.type] = -_

    implicit val realMultiplicationCommutativity: Commutativity[Real, Real, RealMultiplication.type] = {
        new Commutativity[Real, Real, RealMultiplication.type] {}
    }

    implicit val realMultiplicationAssociativity: Associativity[Real, RealMultiplication.type] = {
        new Associativity[Real, RealMultiplication.type]{}
    }

    implicit val realMultiplicationId: Id[Real, RealMultiplication.type] = Id(1)

    implicit val realMultiplicationInversion: NonZeroInversion[Real, RealMultiplication.type, RealAddition.type] = {
        NonZeroInversion(1/_)
    }

    implicit val realDistributivity: Distributivity[Real, RealAddition.type, RealMultiplication.type] = {
        new Distributivity[Real, RealAddition.type, RealMultiplication.type]{}
    }

    implicit val complexAdditionCommutativity: Commutativity[Complex, Complex, ComplexAddition.type] = {
        new Commutativity[Complex, Complex, ComplexAddition.type] {}
    }

    implicit val complexAdditionAssociativity: Associativity[Complex, ComplexAddition.type] = {
        new Associativity[Complex, ComplexAddition.type]{}
    }

    implicit val complexAdditionId: Id[Complex, ComplexAddition.type] = Id(0.r)

    implicit val complexAdditionInversion: FullInversion[Complex, ComplexAddition.type] = -_

    implicit val complexMultiplicationCommutativity: Commutativity[Complex, Complex, ComplexMultiplication.type] = {
        new Commutativity[Complex, Complex, ComplexMultiplication.type] {}
    }

    implicit val complexMultiplicationAssociativity: Associativity[Complex, ComplexMultiplication.type] = {
        new Associativity[Complex, ComplexMultiplication.type]{}
    }

    implicit val complexMultiplicationId: Id[Complex, ComplexMultiplication.type] = Id(1.r)

    implicit val complexMultiplicationInversion
    : NonZeroInversion[Complex, ComplexMultiplication.type, ComplexAddition.type] = NonZeroInversion(1.r/_)

    implicit val complexDistributivity: Distributivity[Complex, ComplexAddition.type, ComplexMultiplication.type] = {
        new Distributivity[Complex, ComplexAddition.type, ComplexMultiplication.type]{}
    }

    implicit val stringConcatenationAssociativity: Associativity[String, StringConcatenation.type] = {
        new Associativity[String, StringConcatenation.type]{}
    }

    implicit val stringConcatenationId: Id[String, StringConcatenation.type] = Id("")
}

object Implicits extends Implicits
