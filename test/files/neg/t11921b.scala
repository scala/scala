

object test1 {

  class C {
    val x = 0
  }
  object Test {
    val x = 1
    class D extends C {
      println(x)  // error
    }
    def f() =
      new C {
        println(x)  // error
      }
  }
}

object test2 {
  def c(y: Float) = {
    class D {
      val y = 2
    }
    new D {
      println(y)  // error
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
        println(y)  // error
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
      val y: Int = this.x // OK
      val z: Int = x      // OK
    }
  }
}

object global

class C {
  val global = 42
}
object D extends C {
  println(global)    // OK, since global is defined in package
}
