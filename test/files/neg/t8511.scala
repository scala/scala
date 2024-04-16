//> using options -Werror
sealed trait Expr
final case class Foo(other: Option[String]) extends Expr
final case class Bar(someConstant: String) extends Expr
final case class Baz() extends Expr
final case class EatsExhaustiveWarning(other: Reference) extends Expr

sealed trait Reference {
  val value: String
}

object Reference {
  def unapply(reference: Reference): Option[(String)] = {
    Some(reference.value)
  }
}

object EntryPoint {
  def main(args: Array[String]) {
    println("Successfully ran")
  }

  private def logic(head: Expr): String = head match {
    case Foo(_) =>
      ???
    // Commenting this line only causes the exhaustive search warning to be emitted
    case EatsExhaustiveWarning(Reference(text)) =>
      ???
  }
}
