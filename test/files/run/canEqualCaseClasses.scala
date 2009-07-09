object Test {
  case class Foo(x: Int)
  case class Bar(y: Int) extends Foo(y)
  case class Nutty() {
    override def canEqual(other: Any) = true
  }

  def assertEqual(x: AnyRef, y: AnyRef) =
    assert((x == y) && (y == x) && (x.hashCode == y.hashCode))

  def assertUnequal(x: AnyRef, y: AnyRef) =
    assert((x != y) && (y != x))

  def main(args: Array[String]): Unit = {
    assertEqual(Foo(5), Foo(5))
    assertEqual(Bar(5), Bar(5))
    assertUnequal(Foo(5), Bar(5))

    // in case there's an overriding implementation
    assert(Nutty() canEqual (new AnyRef))
    assert(!(Foo(5) canEqual (new AnyRef)))
  }
}
