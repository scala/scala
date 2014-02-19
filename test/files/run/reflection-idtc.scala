import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  val tb = cm.mkToolBox()
  val idsym = tb.typecheck(q"type Id[X] = X").symbol.asType
  val idTC1 = idsym.info
  println(idTC1)
  println(appliedType(idTC1, List(typeOf[Int])))
  println("===")
  val idTC2 = idsym.toType.etaExpand
  println(idTC2)
  println(appliedType(idTC2, List(typeOf[Int])))
  println(appliedType(idTC2, List(typeOf[Int])).dealias)
}