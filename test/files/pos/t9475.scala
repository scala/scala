trait Ctx {
  trait Tree
}

trait Lst[+A] {
  def zip[A1 >: A, B](that: Lst[B]): Nothing
}

object Test {

  // both of these methods should be transformed by uncurry
  // such that List[c.Tree] becomes List[Ctx#Tree]:
  def foo1(c: Ctx)(l: Lst[c.Tree]) = l zip l
  def foo2[@specialized T](c: Ctx)(l: Lst[c.Tree], t: T) = l zip l

  // if this doesn't happen for the 2nd method, the specialization
  // transformation fails
}

