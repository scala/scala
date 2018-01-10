import scala.tools.reflect.ToolBox
import scala.reflect.runtime.{ universe => ru }

object Test extends App {
  val tb = scala.reflect.runtime.currentMirror.mkToolBox()
  tb.eval(ru.reify(implicitly[ValueOf[1]]).tree)
}
