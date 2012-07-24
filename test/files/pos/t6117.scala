package test

trait ImportMe {
  def foo(i: Int) = 1
  def foo(s: String) = 2
}

class Test(val importMe: ImportMe) {
  import importMe._
  import importMe._

  // A.scala:12: error: reference to foo is ambiguous;
  // it is imported twice in the same scope by
  // import importMe._
  // and import importMe._
  //   println(foo(1))
  //           ^
  println(foo(1))
}
