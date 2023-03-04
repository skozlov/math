import com.github.skozlov.math.algebra.Implicits._
import com.github.skozlov.math.algebra.operation_properties.associativity.Associativity
import com.github.skozlov.math.algebra.operation_properties.id.Id
import com.github.skozlov.math.algebra.structures.Monoid

object StringConcatenation extends ((String, String) => String) {
  override def apply(a: String, b: String): String = a + b

  override val toString: String = "String concatenation"
}

implicit val stringConcatenationAssociativity
    : Associativity[String, StringConcatenation.type] =
  new Associativity[String, StringConcatenation.type] {}

implicit val stringConcatenationIdentity: Id[String, StringConcatenation.type] =
  Id("")

def printMonoid[A, Op <: (A, A) => A](
    op: Op
)(implicit monoid: Monoid[A, Op]): Unit = {
  println(s"$op is a monoid with identity element = `${monoid.id.id}`")
}

printMonoid[String, StringConcatenation.type](StringConcatenation)
