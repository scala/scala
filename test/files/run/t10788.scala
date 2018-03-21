object Test {
  class Box[T](t: T) {
    def foo: T = {
      println("effect")
      t
    }
  }

  object Box {
    def apply(x: String): Box[x.type] = new Box[x.type](x)
  }

  def main(args: Array[String]): Unit = {
    val bar = Box("foo").foo
  }
}
