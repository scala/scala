object Test {
  def main(args: Array[String]) {
    val o4 = new Outer4
    val o5 = new Outer5
    val o6 = new Outer6

    println(4)
    ser(new o4.Inner(1))
    o4.Inner
    ser(new o4.Inner(1))

    println(2)
    ser(new o5.Inner(1))
    o5.Inner
    ser(new o5.Inner(1))

    println(3)
    ser(new o6.Inner(1))
    o6.Inner
    ser(new o6.Inner(1))

    foo
  }

  def foo {
    case class C(x: Int)
    ser(new C(1))
    ser(C)
  }

  def ser(o: AnyRef) {
    val oos = new java.io.ObjectOutputStream(new java.io.ByteArrayOutputStream())
    oos.writeObject(o)
    oos.close()
  }

}

class Outer4 extends Serializable {
  class Inner(x: Int = 1) extends Serializable
}

class Outer5 extends Serializable {
  case class Inner(x: Int = 1)
}

class Outer6 extends Serializable {
  case class Inner(x: Int)
}
