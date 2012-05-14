class C[@specialized(Int) A](a:A) {
  lazy val b = (a, a)
  def c = b
}
object Test {
  def main(args:Array[String]) {
    println(new C(3).c)
    println(new C(3.0).c)
  }
}
