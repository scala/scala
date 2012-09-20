trait Foo[T <: AnyVal] extends Any {
  def foo(x: String): String
  def foo(x: T): String
}

class Box1(val value: String) extends AnyVal with Foo[Box2] {
  def foo(x: String) = "foo(String): ok"
  def foo(x: Box2) = "foo(Box2): ok"
}

class Box2(val value: String) extends AnyVal


object test2a {

  def main(args: Array[String]) {
    val b1 = new Box1(null)
    val b2 = new Box2(null)
    val f: Foo[Box2] = b1
    println(f.foo(""))
    println(f.foo(b2))
  }
}
