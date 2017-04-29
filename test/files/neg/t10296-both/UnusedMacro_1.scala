
import scala.reflect.macros.whitebox.Context

object UnusedMacro {
  def macroImpl(c: Context)(body: c.Expr[Int]): c.Tree = {
    import c.universe._
    val _ = body
    q"k()"
  }
}
