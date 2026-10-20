//> using jvm 17+
package p

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  // subclasses of classes that are not loaded yet
  def subs: List[List[String]] = macro impl
  def impl(c: Context): c.Tree = {
    import c.universe._
    val names = List("p.Poly", "p.X").map(n => c.mirror.staticClass(n).knownDirectSubclasses.toList.map(_.fullName).sorted)
    q"$names"
  }
}
