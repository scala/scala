// cf. pos/t8300-patmat-a.scala
trait Universe {
  type Name >: Null <: AnyRef with NameApi
  trait NameApi

  type TermName >: Null <: TermNameApi with Name
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