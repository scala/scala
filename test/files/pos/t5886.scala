object A {
  def f0[T](x: T): T = x
  def f1[T](x: => T): T = x
  def f2[T](x: () => T): T = x()

  f0(this.getClass)  // ok
  f1(this.getClass)
  f2(this.getClass)  // ok

  // a.scala:7: error: type mismatch;
  //  found   : Class[_ <: A.type]
  //  required: Class[?0(in value x1)] where type ?0(in value x1) <: A.type
  // Note: A.type >: ?0, but Java-defined class Class is invariant in type T.
  // You may wish to investigate a wildcard type such as `_ >: ?0`. (SLS 3.2.10)
  //   val x1 = f1(this.getClass)
  //                    ^
  // one error found
}
