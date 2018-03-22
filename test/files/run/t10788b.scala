object Test {
  abstract class Box {
    type T
    val t: T
    def foo: T = {
      println("effect")
      t
    }
  }

  object Box {
    def apply(x: String): Box { type T = x.type } = new Box {
      type T = x.type
      val t = x
    }
  }

  def main(args: Array[String]): Unit = {
    val bar = Box("foo").foo
  }
}
