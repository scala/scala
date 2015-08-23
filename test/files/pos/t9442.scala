trait Ctx {
  trait Tree
}
trait Lst[+A] {
  def zip[A1 >: A, B](that: Lst[B]): Nothing
}
class C[@specialized(Int) T] {
  def moo(t: T) = {
    def foo1(c: Ctx)(l: Lst[c.Tree]) = l zip l
    def foo2(c: Ctx)(l: Lst[c.Tree]*) = l(0) zip l(1)
    def foo3(c: Ctx)(l: => Lst[c.Tree]) = l zip l
    ???
  }
}
