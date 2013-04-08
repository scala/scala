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
