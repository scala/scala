class C[E <: Exception] {
  @throws[E] def f = 1

  @throws(classOf[Exception]) def g: E = ???

  @throws[E] @throws[Exception] def h = 1
}

object Test extends App {
  val c = classOf[C[_]]
  def sig(method: String) = c.getDeclaredMethod(method).toString
  def genSig(method: String) = c.getDeclaredMethod(method).toGenericString

  assert(sig("f") == "public int C.f() throws java.lang.Exception")
  assert(genSig("f") == "public int C.f() throws E")

  assert(sig("g") == "public java.lang.Exception C.g() throws java.lang.Exception")
  assert(genSig("g") == "public E C.g() throws java.lang.Exception")

  assert(sig("h") == "public int C.h() throws java.lang.Exception,java.lang.Exception")
  assert(genSig("h") == "public int C.h() throws E,java.lang.Exception")
}
