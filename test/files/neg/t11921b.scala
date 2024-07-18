//> using options -Werror -Xsource:3

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
  println(global)    // error
}

object test5 {
  class A { val x = 1 }
  class B extends A
  object Uhu {
    val x = 2
    class C extends B {
      class Inner {
        def t = x // ambiguous, message mentions parent B
      }
    }
  }
}

object test6 {
  trait I {
    val a = 1
    def a(x: Int) = ""
  }
  class C {
    val a = ""
    trait J extends I {
      val t = a // error
    }
  }
}


object test7 {
  trait T {
    // overloaded a
    val a = ""
    def a(x: Int) = ""
  }

  trait I {
    val a = 1
  }

  class C extends T {
    trait J {
      self: I =>
      // no warning here. when checking for an outer `a`, we find an OverloadedSymbol with the two definitions in `T`.
      // The owner of the overloaded symbol is `C`, but the alternatives have owner `T`.
      val t = a
    }
  }
}

object test9 {
  val lo: Int = 1
  class P {
    implicit val lo: Int = 1
  }
  class C extends P {
    def t(implicit i: Int) = 10
    def u = t // ok, reference to `lo` by implicit search
    def v = t(lo) // should warn, but doesn't. can't tell if reference to `lo` was explicit or not.
  }
}

object test10 {
  implicit val lo: Int = 1
  class P {
    val lo: Int = 1
  }
  class C extends P {
    def t(implicit i: Int) = 10
    def u = t // doesn't compile in Scala 2 (maybe there's a ticket for that)
    def v = t(lo) // error
  }
}

package scala {
  trait P { trait Option[+A] }
  class C extends P {
    def t = new Option[String] {} // OK, competing scala.Option is not defined in the same compilation unit
  }
}

trait t12850 {
  def pm(x: Int) = 1
  def pm(x: String) = 2
}
package object pt12850 extends t12850 {
  def t = pm(1) // no error
}

trait t12850b {
  def pm(x: Int) = 1
  def pm(x: String) = 2
  object O extends t12850b {
    def t = pm(1) // no error
  }
}
