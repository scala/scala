object Test {
  abstract class Box {
    type T
    val t: T
    def foo: T = {
      println("effect")
      t
    }
    def fooEff(): T = {
      println("effect")
      t
    }

    def fooEffPoly[U >: T <: T](): U = {
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

  class BoxParam[T](val t: T) {
    println("ohai")
    def foo: T = {
      println("boo")
      t
    }
  }

  object BoxParam {
    def apply(x: String): BoxParam[x.type] = { println("oh boy"); new BoxParam[x.type](x) }
  }


  class C {
    println("effect42")
    final val const = 42
  }

  def main(args: Array[String]): Unit = {
    val bar = Box("foo").foo
    Box("foo").fooEff()
    Box("foo").fooEffPoly()
    BoxParam("foo").t
    BoxParam("foo").foo

    val x = new C().const
  }
}
