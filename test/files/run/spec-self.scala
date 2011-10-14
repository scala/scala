class Foo0 extends (() => Double) {
  def apply() = 5.0d
}

class Foo1 extends (Double => Double) {
  def apply(x: Double) = x
}

object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo0)())
    println((new Foo1)(5.0d))
  }
}
