object Test {
  def main(args: Array[String]): Unit = {
    println(Macro.classNameOf(classOf[C]))
  }
  class C(val x: Int) extends AnyVal
}
