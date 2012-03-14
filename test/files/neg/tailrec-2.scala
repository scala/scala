sealed abstract class Super[+A] {
  def f[B >: A](mem: List[B]) : List[B]
  def g(mem: List[_]) = ???
}
// This one should fail, target is a supertype
class Bop1[+A](val element: A) extends Super[A] {

  @annotation.tailrec final def f[B >: A](mem: List[B]): List[B] = (null: Super[A]).f(mem)
  @annotation.tailrec final def f1[B >: A](mem: List[B]): List[B] = this.g(mem)
}
// These succeed
class Bop2[+A](val element: A) extends Super[A] {
  @annotation.tailrec final def f[B >: A](mem: List[B]): List[B] = (null: Bop2[A]).f(mem)
}
object Bop3 extends Super[Nothing] {
  @annotation.tailrec final def f[B](mem: List[B]): List[B] = (null: Bop3.type).f(mem)
}
class Bop4[+A](val element: A) extends Super[A] {
  @annotation.tailrec final def f[B >: A](mem: List[B]): List[B] = Other.f[A].f(mem)
}

object Other {
  def f[T] : Bop4[T] = sys.error("")
}

object Bop {
  def m1[A] : Super[A] = sys.error("")
  def m2[A] : Bop2[A] = sys.error("")
}