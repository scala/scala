object T1027 extends App {
  trait Comparable[T <: Comparable[T]] { this: T =>
    def < (that: T): Boolean
    def <=(that: T): Boolean = this < that || this == that
    def > (that: T): Boolean = that < this
    def >=(that: T): Boolean = that <= this
  }
  class A(val x: String) extends Comparable[A]{
    def < (that: A) = this.x < that.x
  }
  val a = new A("a")
  val b = new A("b")
  println(a < b)
  println(a > b)
  println(a <= b)
  println(a >= b)
  println("Comparable traits : " + (new A("x") > new A("y")).toString)
 }
