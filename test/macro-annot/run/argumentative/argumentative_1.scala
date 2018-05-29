import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object argumentativeMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val result = {
      annottees.map(_.tree).toList match {
        case ModuleDef(mods, name, Template(parents, self, body)) :: Nil =>
          val Apply(Select(Apply(_, List(x, y)), _), _) = c.macroApplication
          val toStringMethod = DefDef(Modifiers(OVERRIDE), TermName("toString"), List(), List(List()), TypeTree(), Literal(Constant(s"$x $y")))
          ModuleDef(mods, name, Template(parents, self, body :+ toStringMethod))
      }
    }
    c.Expr[Any](result)
  }
}

class argumentative(val x: Int, y: Int) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro argumentativeMacro.impl
}
