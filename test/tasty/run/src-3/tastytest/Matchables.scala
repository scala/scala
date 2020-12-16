package tastytest

object Matchables {

  def foo: Matchable = true

  def bar[A <: Matchable](a: A) = a match {
    case a: A => a
  }

  class baz[A](val a: A) extends Matchable

  class qux[A <: Matchable](val a: A)

}
