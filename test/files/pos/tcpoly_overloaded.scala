trait Monad[T <: Bound[T], MyType[x <: Bound[x]], Bound[_]] {
  def flatMap[S <: RBound[S], RContainer[x <: RBound[x]], RBound[_],
              Result[x <: RBound[x]] <: Monad[x, RContainer, RBound]]
              (f: T => Result[S]): Result[S]
  def flatMap[S <: RBound[S], RContainer[x <: RBound[x]], RBound[_],
              Result[x <: RBound[x]] <: Monad[x, RContainer, RBound]]
              (f: T => Result[S], foo: String): Result[S]
  def flatMap[S <: Bound[S]]
              (f: T => MyType[S], foo: Int): MyType[S]
}

trait Test {
  def moo: MList[Int]
  class MList[T](el: T) extends Monad[T, List, Any] {
    def flatMap[S <: RBound[S], RContainer[x <: RBound[x]], RBound[_],
            Result[x <: RBound[x]] <: Monad[x, RContainer, RBound]]
            (f: T => Result[S]): Result[S] = sys.error("foo")
    def flatMap[S <: RBound[S], RContainer[x <: RBound[x]], RBound[_],
            Result[x <: RBound[x]] <: Monad[x, RContainer, RBound]]
            (f: T => Result[S], foo: String): Result[S]  = sys.error("foo")
    def flatMap[S]
            (f: T => List[S], foo: Int): List[S]  = sys.error("foo")
  }
  val l: MList[String] = moo.flatMap[String, List, Any, MList]((x: Int) => new MList("String"))
}
