class Foo(val x: Bar) {
  def isEmpty = false
  def get = x
}

object Foo {
  def unapply(x: Foo) = x
}

class Bar(val x: Option[Int], val y: Option[Int]) {
  def isEmpty = false
  def get = this
  def _1 = x
  def _2 = y
}

object Bar {
  def unapply(x: Bar) = x
}

object Test {
  def nameBased: Unit = {
    val x: AnyRef = new Foo(new Bar(Some(1), Some(2)))
    x match {
      case Foo(Bar(x1, x2)) => println(x1)
    }
  }
  def main(args: Array[String]): Unit = {
    nameBased
  }
}
