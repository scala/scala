object Test {
  def main(args: Array[String]) {
    println(Macro.methodTypeOf({def x(a: Int): String = ???}))
    println(Macro.methodTypeOf({def x(): C = ???}))
  }
  class C(val x: Int) extends AnyVal
}
