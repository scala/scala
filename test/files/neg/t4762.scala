// https://issues.scala-lang.org/browse/SI-4762

// In A, x and y are -1.
class A(var x: Int) {
  val y: Int = -1
}

// In B, x and y are 99 and private[this], implicitly so
// in the case of x.
class B(x: Int) extends A(-1) {
  private[this] def y: Int = 99

  // Three distinct results.
  def f = List(
    /* (99,99) */  (this.x, this.y),
    /* (-1,99) */  ((this: B).x, (this: B).y),
    /* (-1,-1) */  ((this: A).x, (this: A).y)
  )

  // The 99s tell us we are reading the private[this]
  // data of a different instance.
  def g(b: B) = List(
    /* (-1,99) */  (b.x, b.y),
    /* (-1,99) */  ((b: B).x, (b: B).y),
    /* (-1,-1) */  ((b: A).x, (b: A).y)
  )
}

object Test {
  def f(x: A)  = /* -2 */  x.x + x.y
  def g1(x: B) = /* -2 */  (x: A).x + (x: A).y
  def g2(x: B) = (x: B).x + (x: B).y
  // java.lang.IllegalAccessError: tried to access method B.y()I from class Test$

  def main(args: Array[String]): Unit = {
    val b = new B(99)
    b.f foreach println
    b.g(new B(99)) foreach println

    println(f(b))
    println(g1(b))
    println(g2(b))
  }
}

class bug4762 {
  class Base( var x : Int ) { def increment() { x = x + 1 } }
  class Derived( x : Int ) extends Base( x ) { override def toString = x.toString }

  val derived = new Derived( 1 )
}
