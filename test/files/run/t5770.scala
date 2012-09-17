import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect._

object Test extends App {
  var i = 0
  val action = reify { i += 1; println(i) }.tree

  val tb1 = cm.mkToolBox()
  tb1.eval(action)
  tb1.eval(action)
  tb1.eval(action)
  tb1.frontEnd.reset()
  tb1.eval(action)
  tb1.eval(action)

  val tb2 = cm.mkToolBox()
  tb2.eval(action)
  tb2.frontEnd.reset()
  tb2.eval(action)
  tb2.eval(action)
  tb2.frontEnd.reset()
  tb2.eval(action)
  tb2.eval(action)
}
