//> using options -Ymacro-annotations
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.StaticAnnotation

object Macros {
  def annotImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val classTree = annottees.head.tree
    val objectTree = q"""
    object X {
      def f: Int => String = { x => "hello" }
    }
    """

    c.Expr[Any](Block(List(classTree, objectTree), Literal(Constant(()))))
  }
}

class mymacro extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.annotImpl
}
