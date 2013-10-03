import scala.language.higherKinds

trait Bound[B]

package p1 {
  case class Sub[B <: Bound[B]](p: B)
  object Test {
    def g[A](x: Bound[A]) = ()
    def f(x: Any) = x match { case Sub(p) => g(p) }
  }
}

package p2 {
  trait Traversable[+A] { def head: A = ??? }
  trait Seq[+A] extends Traversable[A] { def length: Int = ??? }

  case class SubHK[B <: Bound[B], CC[X] <: Traversable[X]](xs: CC[B])
  class MyBound extends Bound[MyBound]
  class MySeq extends Seq[MyBound]

  object Test {
    def g[B](x: Bound[B]) = ()

    def f1(x: Any) = x match { case SubHK(xs) => xs }
    def f2[B <: Bound[B], CC[X] <: Traversable[X]](sub: SubHK[B, CC]): CC[B] = sub match { case SubHK(xs) => xs }
    def f3 = g(f1(SubHK(new MySeq)).head)
    def f4 = g(f2(SubHK(new MySeq)).head)
  }
}
