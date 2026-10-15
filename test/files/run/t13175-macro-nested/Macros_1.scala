//> using jvm 17+
package p

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  // direct subclasses, recursing into sealed children
  def tree[T]: List[String] = macro impl[T]
  def impl[T: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._
    def loop(sym: ClassSymbol, indent: String): List[String] =
      s"$indent${sym.fullName}" :: sym.knownDirectSubclasses.toList.sortBy(_.fullName).flatMap(s => loop(s.asClass, indent + "  "))
    q"${loop(weakTypeOf[T].typeSymbol.asClass, "")}"
  }
}
