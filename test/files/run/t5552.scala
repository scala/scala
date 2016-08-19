class C[@specialized(Int) A](a:A) {
  lazy val b = {println(s"lazy: $a"); (a, a)} // there should only be two instances of "lazy" in the output
  def c = b
}
object Test {
  def main(args:Array[String]) {
    val cInt = new C(3)
    println(cInt.c)
    println(cInt.c)
    val cFloat = new C(3.0)
    println(cFloat.c)
    println(cFloat.c)
  }
}
