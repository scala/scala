object WhatsYourTypeIsMyType {
  trait WithMyType[+T] {
    type MyType = T
  }

  class Foo extends WithMyType[Foo] {
    var x: MyType = _
    def setX() = x = new Foo
  }

  class Bar extends Foo with WithMyType[Bar] {
    def unsound { println("iAmABar") }

    setX()
    println(x.unsound)
  }

  def main(args: Array[String]): Unit = new Bar
}
