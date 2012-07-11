sealed trait Foo
class Bar1 extends Foo
class Bar2 extends Foo
class Bar3 extends Foo

// these extractors are known to always succeed as they return a Some
object Baz1 {
  def unapply(x: Bar1): Some[Int] = Some(1)
}
object Baz2 {
  def unapply(x: Bar2): Some[Int] = Some(2)
}


object Test {
  // warning: missing Bar3
  def f1(x: Foo) = x match {
    case _: Bar1 => 1
    case _: Bar2 => 2
  }

  // warning: missing Bar3
  def f2(x: Foo) = x match {
    case _: Bar1 => 1
    case Baz2(x) => x
  }

  // warning: missing Bar3
  def f3(x: Foo) = x match {
    case Baz1(x) => x
    case Baz2(x) => x
  }
}