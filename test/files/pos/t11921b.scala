// scalac: -Xfatal-warnings -Xsource:2.13 -Wconf:msg=legacy-binding:s

object test1 {

  class C {
    val x = 0
  }
  object Test {
    val x = 1
    class D extends C {
      println(x)
    }
    def f() =
      new C {
        println(x)
      }
  }
}

object test2 {
  def c(y: Float) = {
    class D {
      val y = 2
    }
    new D {
      println(y)
    }
  }
}

object test3 {
  def c(y: Float) = {
    class D {
      val y = 2
    }
    class E extends D {
      class F {
        println(y)
      }
    }
  }
}

object test4 {

  class C {
    val x = 0
  }
  object Test {
    val x = 1
    class D extends C {
      def x(y: Int) = 3
      val y: Int = this.x
      val z: Int = x
    }
  }
}

object global

class C {
  val global = 42
}
object D extends C {
  println(global)
}

object test5 {
  trait T {
    val s: String
  }

  trait U {
    this: T =>
    val s: String
    def t = s
  }


  class AA {
    def f = 1
    class B extends AA {
      def g = f
    }
  }
}
