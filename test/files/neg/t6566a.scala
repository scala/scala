object WhatsYourTypeIsMyType {
  class TypeCheat[+T] { type MyType = T }

  class Foo {
    val tc = new TypeCheat[Foo]
    var x: tc.MyType = _
    def setX() = x = new Foo
  }
  class Bar extends Foo {
    override val tc = new TypeCheat[Bar]
    def unsound = this

    setX()
    println(x.unsound)
  }
  def main(args: Array[String]): Unit = new Bar
}
