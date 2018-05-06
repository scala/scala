// http://stackoverflow.com/questions/22549647/whats-the-right-way-to-generate-a-companion-class-de-novo-using-quasiquotes
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object thingyMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val toEmit = c.Expr(q"""
class Thingy(i: Int) {
  def stuff = println(i)
}

object Thingy {
  def apply(x: Int) = new Thingy(x)
}
""")
    annottees.map(_.tree) match {
      case Nil => {
        c.abort(c.enclosingPosition, "No test target")
      }
      case (classDeclaration: ClassDef) :: Nil => {
        // println("No companion provided")
        toEmit
      }
      case (classDeclaration: ClassDef) :: (companionDeclaration: ModuleDef) :: Nil => {
        // println("Companion provided")
        toEmit
      }
      case _ => c.abort(c.enclosingPosition, "Invalid test target")
    }
  }
}

class thingy extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro thingyMacro.impl
}

package pkg {
  class thingy extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro thingyMacro.impl
  }
}
