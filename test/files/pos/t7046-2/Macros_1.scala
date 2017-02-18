package p1

import scala.reflect.macros.blackbox._
import language.experimental._

object Macro {
  def impl(c: Context): c.Tree = {
    import c.universe._
    val tsym = rootMirror.staticClass("p1.Base")
    val subclasses = tsym.knownDirectSubclasses.toList.map(_.name.toString)
    q"$subclasses"
  }
  def p1_Base_knownDirectSubclasses: List[String] = macro impl
}
