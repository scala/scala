case class A(private var foo: Any) {
  def m = { def foo = 42 /*will be lamba lifted to `A#foo$1`*/ }
}
object Test {
  def main(args: Array[String]): Unit = {
    val A("") = new A("")  
    new A("").m
  }
}
