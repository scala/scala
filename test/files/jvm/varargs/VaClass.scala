

import annotation.varargs



class VaClass {

  @varargs def vs(a: Int, b: String*) = println(a + b.length)
  @varargs def vi(a: Int, b: Int*) = println(a + b.sum)

}
