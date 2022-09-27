object NoMoreNeg {
  def foo(x: AnyRef): x.type = x
  val x: AnyRef => Any = foo
}

object t12641 {
  def f(sb: StringBuilder) = Option("").foreach(sb.append)
}

object t12641a {
  trait A {
    def foo(s: String): this.type
    def foo(s: Int): this.type
  }
  trait T {
    val a1: A
    val o: Option[String]

    def t(a2: A): Unit = {
      o.foreach(a1.foo)
      o.foreach(a2.foo)

      val f2: String => a2.type = a2.foo
      val f3: String => A = a2.foo
    }
  }
}

object t12641b {
  trait A {
    def foo(s: String): this.type
  }
  trait T {
    val a1: A
    val o: Option[String]

    def t(a2: A): Unit = {
      o.foreach(a1.foo)
      o.foreach(a2.foo)

      val f1 = a2.foo _
      val f2: String => a2.type = a2.foo
      val f3: String => A = a2.foo
    }
  }
}

import scala.tools.partest._

object Test extends ReplTest with Lambdaless {
  def code = """
object defs {
  val a = "obj"
  def aa: a.type = a
  def s = this
  def f(x: Int): a.type = a
  def g(x: Int)(y: x.type) = 0
  def h(x: a.type)(y: a.type) = 0
}
import defs._
val f1 = f _
val f2: Int => a.type = f
val f3: Int => Object = f
val g1 = g(10) _
val g2: 10 => Int = g1
val g3: 11 => Int = g(11)
val g4: Int => Int = g(11) // mismatch
val h1 = s.h(aa) _
val h2: a.type => Int = h1
val h3: a.type => Int = s.h(aa)
""".trim
}
