object Test {
  def main(args: Array[String]) {
    println(Macro.classNameOf(classOf[C]))
  }
  class C(val x: Int) extends AnyVal
}
