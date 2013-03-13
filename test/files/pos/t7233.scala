object Foo {
  def bar(i: Int) = i

  def ol(i: Int) = i
  def ol(i: String) = i
}
object Test {
  import Foo.{ bar => quux, toString => bar, ol => olRenamed}

  val f1 = quux _
  val f1Typed: (Int => Int) = f1

  val f2: String => String = olRenamed _
}
