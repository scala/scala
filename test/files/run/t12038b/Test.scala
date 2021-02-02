object Test {
  def main(args: Array[String]): Unit = {
    import reflect.runtime.universe._
    val m = scala.reflect.runtime.currentMirror
    println(m.staticClass("p1.J_1").companion.info.decl(TermName("test")).asMethod.info.toString)
  }
}
