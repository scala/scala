import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object identityMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    c.Expr[Any](Block(annottees.map(_.tree).toList, Literal(Constant(()))))
  }
}

class identity extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro identityMacro.impl
}

class identity1 extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro identityMacro.impl
}

package pkg {
  class identity extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro identityMacro.impl
  }

  class identity2 extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro identityMacro.impl
  }

  object Module3 {
    class identity3 extends StaticAnnotation {
      def macroTransform(annottees: Any*): Any = macro identityMacro.impl
    }
  }
}

object Module4 {
  class identity4 extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro identityMacro.impl
  }
}
