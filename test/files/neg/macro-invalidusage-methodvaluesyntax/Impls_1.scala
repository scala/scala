// scalac: -language:experimental.macros
import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context) = {
    import c.universe._
    c.Expr[Unit](q"""println("it works")""")
  }
}
