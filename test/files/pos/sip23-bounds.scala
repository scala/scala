object Test {
  class Covar[-A]

  def foo[A, CC[X] <: Option[X]]: Covar[CC[_ <: A]] = ???

  val bar: Covar[Option[Int]] = foo
}
