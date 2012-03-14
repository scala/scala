// More shoddy treatment for nothing.
class A {
  class Q3A[+T1, T2 <: T1](x: T1)
  class Q3B[+T1, T2 <: T1](x: Q3A[T1, T2])

  val x1 = new Q3B(new Q3A("a"))
  val x2 = new Q3B(new Q3A[String, Nothing]("a"))
  val x3 = new Q3B(new Q3A[String, Null]("a"))
  // test/pending/pos/nothing.scala:5: error: type mismatch;
  //  found   : A.this.Q3A[String,Nothing]
  //  required: A.this.Q3A[String,T2]
  // Note: Nothing <: T2, but class Q3A is invariant in type T2.
  // You may wish to define T2 as +T2 instead. (SLS 4.5)
  //   val x1 = new Q3B(new Q3A("a"))
  //                    ^
  // test/pending/pos/nothing.scala:6: error: type mismatch;
  //  found   : A.this.Q3A[String,Nothing]
  //  required: A.this.Q3A[String,T2]
  // Note: Nothing <: T2, but class Q3A is invariant in type T2.
  // You may wish to define T2 as +T2 instead. (SLS 4.5)
  //   val x2 = new Q3B(new Q3A[String, Nothing]("a"))
  //                    ^
  // two errors found
}
