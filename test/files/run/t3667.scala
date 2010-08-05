object Test {
  def main(args: Array[String]) {
    val o1 = new Outer1
    val o2 = new Outer2
    val o3 = new Outer3

    println(1)
    ser(new o1.Inner(1))
    o1.Inner // make sure the Inner$module field of the Outer1 instance is initialized!
    ser(new o1.Inner(1))

    println(2)
    ser(new o2.Inner(1))
    o2.Inner
    ser(new o2.Inner(1))

    println(3)
    ser(new o3.Inner(1))
    o3.Inner
    ser(new o3.Inner(1))
  }

  def ser(o: AnyRef) {
    val oos = new java.io.ObjectOutputStream(new java.io.ByteArrayOutputStream())
    oos.writeObject(o)
    oos.close()
  }

}

@serializable
class Outer1 {
  @serializable
  class Inner(x: Int = 1)
}

@serializable
class Outer2 {
  case class Inner(x: Int = 1)
}

@serializable
class Outer3 {
  case class Inner(x: Int)
}
