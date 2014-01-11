import scala.reflect.macros.blackbox.Context

object B { override def toString = "object" }
class C { override def toString = "class" }

package foo {
  object B { override def toString = "package > object" }
  class C { override def toString = "package > class" }
}

object foo {
  object B { override def toString = "object > object" }
  class C { override def toString = "object > class" }
}

object packageless {
  def impl(c: Context) = {
    import c.universe._
    reify {
      println(B)
      println(new C)
      println(foo.B)
      println(new foo.C)
      println(_root_.foo.B)
      println(new _root_.foo.C)
    }
  }

  def test = macro impl
}

package packageful {
  object Test {
    def impl(c: Context) = {
      import c.universe._
      reify {
        println(B)
        println(new C)
        println(foo.B)
        println(new foo.C)
        println(_root_.foo.B)
        println(new _root_.foo.C)
      }
    }

    def test = macro impl
  }
}
