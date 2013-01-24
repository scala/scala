object Test {
  abstract class <~<[-From, +To] extends (From => To)
  implicit def trivial[A]: A <~< A = sys.error("")


  trait Forcible[T]
  implicit val forcibleInt: (Int <~< Forcible[Int]) = sys.error("")

  def headProxy[P <: Forcible[Int]](implicit w: Int <~< P): P = sys.error("")

  headProxy
  // trivial[Int] should not be considered a valid implicit, since w would have type Int <~< Int,
  // and headProxy's type parameter P cannot be instantiated to Int
}
