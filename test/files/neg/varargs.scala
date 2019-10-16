import annotation.varargs

// Failing varargs annotation
object Test {

  trait A {
    def v1(a: Int, b: Array[String]) = a
  }

  trait B extends A {
    @varargs def v1(a: Int, b: String*) = a + b.length // nok
  }

  @varargs def nov(a: Int) = 0 // nok
  @varargs def v(a: Int, b: String*) = a + b.length // ok
  @varargs def v2(a: Int, b: String*) = 0 // nok
  def v2(a: Int, b: Array[String]) = 0

  @varargs def v3(a: String*)(b: Int) = b + a.length // nok
  @varargs def v4(a: String)(b: Int) = b + a.length // nok
  @varargs def v5(a: String)(b: Int*) = a + b.sum // ok

  @varargs def v6: Int = 1 // nok
  @varargs def v7(i: Int*)() = i.sum // ok (?)

}
