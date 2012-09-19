trait Foo extends Any {
  def box1(x: Box1): String
  def box2(x: Box2): String
}

class Box1(val value: String) extends AnyVal

class Box2(val value: String) extends AnyVal with Foo {
  def box1(x: Box1) = "box1: ok"
  def box2(x: Box2) = "box2: ok"
}

class C(x: String) {
  def this() = this("")
}

object Test {

  def main(args: Array[String]) {
    val b1 = new Box1("")
    val b2 = new Box2("")
    val f: Foo = b2
    println(f.box1(b1))
    println(f.box2(b2))
  }
}
