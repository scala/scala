// cf. pos/t8300-patmat-b.scala
trait Universe {
  type Name >: Null <: AnyRef with NameApi
  trait NameApi

  type TermName >: Null <: Name with TermNameApi
  trait TermNameApi extends NameApi
}

object Test extends App {
  val u: Universe = ???
  import u._

  locally {
    val ScalaName: TermName = ???
    ??? match {
      case ScalaName => ???
    }
  }
}