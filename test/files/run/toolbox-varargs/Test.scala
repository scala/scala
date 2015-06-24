object Test {
  def main(args: Array[String]): Unit = {
     import scala.tools.reflect.ToolBox
     val m = reflect.runtime.currentMirror
     val u = m.universe
     import u._
     val tb = m.mkToolBox();
     tb.compile(q"new p.Varargs(null, null)")
     tb.compile(q"p.Varargs.staticMethod(null, null)")
     tb.compile(q"(null: p.Varargs).instanceMethod(null, null)")
  }
}

