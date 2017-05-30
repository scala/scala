object Test {
  def main(args: Array[String]): Unit = {
    import scala.tools.reflect.ToolBox
    val m = scala.reflect.runtime.currentMirror
    val u = m.universe
    import u._
    val tb = m.mkToolBox(options = "-useurlcp")

    { import p1.p2; new p2.A }
    tb.eval(q"import p1.p2; new p2.A")
    tb.eval(q"List(1, 2, 3)")
  }
}
