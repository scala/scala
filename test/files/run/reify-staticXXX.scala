import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

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
  def test = {
    println(B)
    println(reify(B).eval)
    println(new C)
    println(reify(new C).eval)
    println(foo.B)
    println(reify(foo.B).eval)
    println(new foo.C)
    println(reify(new foo.C).eval)
    println(_root_.foo.B)
    println(reify(_root_.foo.B).eval)
    println(new _root_.foo.C)
    println(reify(new _root_.foo.C).eval)
  }
}

package packageful {
  object Test {
    def test = {
      println(B)
      println(reify(B).eval)
      println(new C)
      println(reify(new C).eval)
      println(foo.B)
      println(reify(foo.B).eval)
      println(new foo.C)
      println(reify(new foo.C).eval)
      println(_root_.foo.B)
      println(reify(_root_.foo.B).eval)
      println(new _root_.foo.C)
      println(reify(new _root_.foo.C).eval)
    }
  }
}

object Test extends App {
  packageless.test
  packageful.Test.test
}
