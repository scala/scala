// SI-6675 DEPRECATED AUTO-TUPLING BECAUSE BAD IDEA -- MEAMAXIMACULPA
// TODO: remove this test case in 2.12, when the deprecation will go into effect and this will no longer compile
// slightly overkill, but a good test case for implicit resolution in extractor calls,
// along with the real fix: an extractor pattern with 1 sub-pattern should type check for all extractors
// that return Option[T], whatever T (even if it's a tuple)
object Foo {
  def unapply[S, T](scrutinee: S)(implicit witness: FooHasType[S, T]): Option[T] = scrutinee match {
    case i: Int => Some((i, i).asInstanceOf[T])
  }
}

class FooHasType[S, T]
object FooHasType {
  implicit object int extends FooHasType[Int, (Int, Int)]
}

// resurrected from neg/t997
object Foo997 { def unapply(x : String): Option[(String, String)] = Some((x, x)) }

object Test extends App {
  val x = 8
  println(x match {
    case Foo(p) => p // p should be a pair of Int
  })

  // Prints '(x, x)'
  "x" match { case Foo997(a) => println(a) }
}