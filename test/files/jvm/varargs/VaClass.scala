import annotation.varargs

class VaClass {
  @varargs def vs(a: Int, b: String*) = println(a + b.length)
  @varargs def vi(a: Int, b: Int*) = println(a + b.sum)
  @varargs def vt[T](a: Int, b: T*) = println(a + b.length)
  @varargs def vt1[T](a: Int, b: T*): T = b.head
}
