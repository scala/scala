// This test case minimizes a case that failed to compile due to a bug in my work on
// scala/bug#5294. After refining my patches, it compiles again, as expected.
class Test {
  trait Tree
  def foo(b: Boolean, buf: collection.mutable.ArrayBuffer[Any], acc: StringBuilder) = if (b) buf else acc

  // In the 2.12 collection implementation, `bar` and `baz` used to produce ill-bounded LUBs.
  // In the 2.13 collections, they work fine, but the underlying bug still exists. See `run/invalid-lubs.scala`
  def bar(a: Boolean, b: collection.mutable.Set[Any], c: collection.mutable.ListBuffer[Any]) = if (a) b else c
  def baz(a: Boolean, b: scala.collection.mutable.SetOps[Any,scala.collection.mutable.Set,scala.collection.mutable.Set[Any]], c: scala.collection.mutable.Buffer[Any]) = if (a) b else c
}
