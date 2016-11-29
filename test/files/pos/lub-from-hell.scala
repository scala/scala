class Test {
  trait Tree
  def foo(b: Boolean, buf: collection.mutable.ArrayBuffer[Any], acc: StringBuilder) = if (b) buf else acc
}
// This test case minimizes a case that failed to compile due to a bug in my work on
// SI-5294. After refining my patches, it compiles again, as expected.