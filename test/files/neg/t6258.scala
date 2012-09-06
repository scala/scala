object Test {
	val f : PartialFunction[_, Int] = { case a : Int => a } // undefined param

	def foo[A](pf: PartialFunction[A, Int]) {};
	foo { case a : Int => a } // undefined param

	val g : PartialFunction[Int, _] = { case a : Int => a } // okay
}


// Another variation, seen in the wild with Specs2.
class X {
  trait Matcher[-T]

  def bar[T](m: Matcher[T]) = null
  def bar[T](i: Int) = null

  def foo[T](p: PartialFunction[T, Any]): Matcher[T] = null

  case class M[X](a: X)

  bar[M[Any]] (foo { // undefined param
    case M(_) => null
  })
}
