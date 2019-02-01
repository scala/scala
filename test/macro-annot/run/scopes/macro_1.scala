import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object explorerMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    def explore() = {
      val a = c.mirror.staticModule("A")
      val b = c.mirror.staticModule("B")
      val i = c.inferImplicitValue(typeOf[Int])
      "can see A, can see B, implicit is " + i
    }
    val result = {
      annottees.map(_.tree).toList match {
        case ModuleDef(mods, name, Template(parents, self, body)) :: Nil =>
          val toStringMethod = DefDef(Modifiers(OVERRIDE), TermName("toString"), List(), List(List()), TypeTree(), Literal(Constant(explore())))
          ModuleDef(mods, name, Template(parents, self, body :+ toStringMethod))
      }
    }
    c.Expr[Any](result)
  }
}

class explorer extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro explorerMacro.impl
}

package pkg {
  class explorer extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro explorerMacro.impl
  }
}
