// scalac: -Xfatal-warnings -Xsource:2.13 -Wconf:msg=legacy-binding:s

object test1 {

  class C {
    val x = 0
  }
  object Test {
    val x = 1
    class D extends C {
      println(x) // error
    }
    def f() =
      new C {
        println(x) // error
      }
  }
}

object test2 {
  def c(y: Float) = {
    class D {
      val y = 2
    }
    new D {
      println(y) // error
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
        println(y) // error
      }
    }
  }
}

object global

class C {
  val global = 42
}
object D extends C {
  println(global) // error
}
