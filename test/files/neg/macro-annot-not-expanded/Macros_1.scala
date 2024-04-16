//> using options -Ymacro-annotations
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.annotation.StaticAnnotation

object Macros {
  def annotImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = ???
}

class annot extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.annotImpl
}
