// The test which this should perform but does not
// is that f1 is recognized as irrefutable and f2 is not
// This can be recognized via the generated classes:
//
// A$$anonfun$f1$1.class
// A$$anonfun$f2$1.class
// A$$anonfun$f2$2.class
//
// The extra one in $f2$ is the filter.
//
// !!! Marking with exclamation points so maybe someday
// this test will be finished.
class A {
  case class Foo[T](x: T)

  def f1(xs: List[Foo[Int]]) = {
    for (Foo(x: Int) <- xs) yield x
  }
  def f2(xs: List[Foo[Any]]) = {
    for (Foo(x: Int) <- xs) yield x
  }
}
