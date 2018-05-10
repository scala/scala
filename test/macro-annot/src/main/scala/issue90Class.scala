import scala.annotation.compileTimeOnly
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object issue90Macro {
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
        c.Expr(EmptyTree)
    }
}

@compileTimeOnly("this is the only annotation")
final class issue90Class extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro issue90Macro.impl
}
