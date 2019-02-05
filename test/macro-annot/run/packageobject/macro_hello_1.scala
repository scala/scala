import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object helloMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case ModuleDef(mods, name, Template(parents, self, body)) :: Nil =>
          val helloMethod = DefDef(NoMods, TermName("hello"), List(), List(List()), TypeTree(), Literal(Constant("hello")))
          ModuleDef(mods, name, Template(parents, self, body :+ helloMethod))
      }
    }
    c.Expr[Any](result)
  }
}

class hello extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro helloMacro.impl
}

package pkg {
  class hello extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro helloMacro.impl
  }
}
