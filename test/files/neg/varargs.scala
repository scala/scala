


import annotation.varargs



// Failing varargs annotation
object Test {

  @varargs def nov(a: Int) = 0
  @varargs def v(a: Int, b: String*) = a + b.length
  @varargs def v2(a: Int, b: String*) = 0
  def v2(a: Int, b: Array[String]) = 0

  def main(args: Array[String]) {

  }

}
