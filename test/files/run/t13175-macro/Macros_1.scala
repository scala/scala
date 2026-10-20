//> using jvm 17+
package p

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def subs[T]: List[String] = macro impl[T]
  def impl[T: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    val names = weakTypeOf[T].typeSymbol.asClass.knownDirectSubclasses.toList.map(_.fullName).sorted
    q"$names"
  }
}
