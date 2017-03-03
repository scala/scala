class Test {
  def m(i: Int) = i

  def expectWild[A](f: A) = ???
  def expectFun[A](f: A => Int) = ???

  expectWild((i => m(i))) // manual eta expansion
  expectWild(m(_)) // have to undo eta expansion with wildcard expected type
  expectFun(m(_)) // have to undo eta expansion with function expected type
}
