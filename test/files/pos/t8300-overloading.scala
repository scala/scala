// cf. neg/t8300-overloading.scala
trait Universe {
  type Name >: Null <: AnyRef with NameApi
  trait NameApi

  type TermName >: Null <: TermNameApi with Name
  trait TermNameApi extends NameApi
}

object Test extends App {
  val u: Universe = ???
  import u._

  def foo(name: Name) = ???
  def foo(name: TermName) = ???
}