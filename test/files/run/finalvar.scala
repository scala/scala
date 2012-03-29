object Final {
  class X(final var x: Int)  { }
  def f = new X(0).x += 1
}

class A {
  var x = 1
  def y0 = x
  def y1 = this.x
  def y2 = (this: A).x
}

class B extends A {
  override def x = 2
  def z = super.x
}

object Test {
  def main(args: Array[String]): Unit = {
    Final.f
    val a = new B
    println((a.x, a.y0, a.y1, a.y2, a.z))
    val a0: A = a
    println((a0.x, a0.y0, a0.y1, a0.y2))
    a.x = 1001
    println((a.x, a.y0, a.y1, a.y2, a.z))
    println((a0.x, a0.y0, a0.y1, a0.y2))

    val d = new D
    println(d.w)
    d.ten
    println(d.w)
  }
}

class C { var w = 1 ; def ten = this.w = 10 }
class D extends C { override var w = 2 }