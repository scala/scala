object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  import scala.tools.reflect.ToolBox
  val tb = cm.mkToolBox()
  val t = { var x = "ab".toList; u.reify { x = x.reverse; x }.tree }
  val evaluated = tb.eval(t)
  assert(evaluated == "ba".toList, evaluated)
}
