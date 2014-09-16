

import annotation.varargs



class VaClass {

  @varargs def vs(a: Int, b: String*) = println(a + b.length)
  @varargs def vi(a: Int, b: Int*) = println(a + b.sum)
  @varargs def vt[T](a: Int, b: T*) = println(a + b.length)

  // TODO remove type bound after fixing SI-8786, see also https://github.com/scala/scala/pull/3961
  @varargs def vt1[T <: String](a: Int, b: T*): T = b.head
}
