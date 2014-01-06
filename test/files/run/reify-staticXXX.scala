import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object B { override def toString = "object" }
class C { override def toString = "class" }

package foo1 {
  object B { override def toString = "package > object" }
  class C { override def toString = "package > class" }
}

object Foo2 {
  object B { override def toString = "object > object" }
  class C { override def toString = "object > class" }
}

object packageless {
  def test = {
    println(B)
    println(reify(B).eval)
    println(new C)
    println(reify(new C).eval)
    println(Foo2.B)
    println(reify(Foo2.B).eval)
    println(new Foo2.C)
    println(reify(new Foo2.C).eval)
    println(_root_.foo1.B)
    println(reify(_root_.foo1.B).eval)
    println(new _root_.foo1.C)
    println(reify(new _root_.foo1.C).eval)
  }
}

package packageful {
  object Test {
    def test = {
      println(B)
      println(reify(B).eval)
      println(new C)
      println(reify(new C).eval)
      println(Foo2.B)
      println(reify(Foo2.B).eval)
      println(new Foo2.C)
      println(reify(new Foo2.C).eval)
      println(_root_.foo1.B)
      println(reify(_root_.foo1.B).eval)
      println(new _root_.foo1.C)
      println(reify(new _root_.foo1.C).eval)
    }
  }
}

object Test extends App {
  packageless.test
  packageful.Test.test
}
