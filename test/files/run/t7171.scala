trait T {
  final case class A()

  // Was:
  // error: scrutinee is incompatible with pattern type;
  // found   : T.this.A
  // required: T#A
  def foo(a: T#A) = a match {
    case _: A => true; case _ => false
  }
}

object Test extends App {
  val t1 = new T {}
  val t2 = new T {}
  val a1 = new t1.A()
  val a2 = new t1.A()
  assert(t1.foo(a1))
  // as noted in the unchecked warning (also tested in the corresponding neg test),
  // the outer pointer isn't checked
  assert(t1.foo(a2))
}
