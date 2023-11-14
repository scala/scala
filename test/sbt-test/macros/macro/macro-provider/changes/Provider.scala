package example
import scala.language.experimental.macros
import scala.reflect.macros._

object Provider {
  def tree(args: Any): Any = macro treeImpl
  def treeImpl(c: Context)(args: c.Expr[Any]): c.Expr[Any] = sys.error("no macro for you!")
}
