
object Test extends Function0[Int] {
  // this and v resolve to Test.this, Test.v not A.this, A.v
  class A(x: Function0[Int] = this)(val a: Int = v, val b: Int = v * x()) extends Function0[Int] {
    val v = 3
    override def toString = x.toString +", "+ a +", "+ b
    // ordinary instance scope
    def m(i: Int = v, y: Function0[Int] = this) = "m, "+ i +", "+ y()
    def apply() = 19
  }
  object A {
    val v = 5
    // should happily coexist with default getters, in a happier world
    def init(x: Function0[Int] = Test.this)(a: Int = v, b: Int = v * x()) = x.toString +", "+ a +", "+ b
    override def toString = "A"
  }
  val v = 7
  def apply() = 17
  override def toString = "Test"
  def main(args: Array[String]) {
    val sut = new A()()
    println(sut.toString)
    println(sut.m())
    println(A.init()())

    println((new T.C()).x)
    println((new T.D(0,0)).x)
  }
}

object T {
  override def toString = "T"

  // `this` refers to T
  class C(val x: Any = {println(this); this}) { // prints T
    println(this) // prints C
    override def toString() = "C"
  }

  class D(val x: Any) {
    override def toString() = "D"
    // `this` refers again to T
    def this(a: Int, b: Int, c: Any = {println(this); this}) { this(c); println(this) } // prints T, then prints D
  }
}
