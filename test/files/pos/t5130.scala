import scala.language.reflectiveCalls

class A {
  this_a =>

  def b = new B
  class B { def a: this_a.type = this_a }
}
trait A2 { def c = () }

object Test {
  val v1 = new A { def c = () }
  val v2 = new A with A2 { }
  val v3: A { def c: Unit } = null
  def d1 = new A { def c = () }
  def d2 = new A with A2 { }
  def d3: A { def c: Unit } = null
  var x1 = new A { def c = () }
  var x2 = new A with A2 { }
  var x3: A { def c: Unit } = null

  def main(args: Array[String]): Unit = {
    val mv1 = new A { def c = () }
    val mv2 = new A with A2 { }
    val mv3: A { def c: Unit } = null
    def md1 = new A { def c = () }
    def md2 = new A with A2 { }
    def md3: A { def c: Unit } = null

     v1.b.a.c
     v2.b.a.c
     v3.b.a.c
     d1.b.a.c
     d2.b.a.c
     d3.b.a.c
     x1.b.a.c
     x2.b.a.c
     x3.b.a.c
    mv1.b.a.c
    mv2.b.a.c
    mv3.b.a.c
    md1.b.a.c
    md2.b.a.c
    md3.b.a.c
  }
}
