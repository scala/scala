import scala.language.experimental.macros
import scala.reflect.macros.Context

object A {
  def hello_impl(c: Context): c.Expr[Unit] = {
    import c.universe._
    reify {
      println("hello world")
    }
  }
}

class B{
  def hello = macro A.hello_impl
}