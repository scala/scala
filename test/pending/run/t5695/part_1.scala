import language.experimental.macros
import scala.reflect.makro.Context

object Defs {

  def mkInt = macro mkIntImpl
  def mkIntImpl(c: Context): c.Expr[Any] = {
    println(c.enclosingApplication)
    c.reify{ 23 }
  }

}
