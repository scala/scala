import language.experimental.macros
import language.implicitConversions
import scala.reflect.macros.whitebox.Context

class C

class RichC { def rich = "" }
object Macro {
  implicit def m: String = macro impl
  def impl(c: Context): c.Tree = {
    import c.universe._
    Literal(Constant("hi"))
  }

  implicit def v(x: C): RichC = macro impl2
  def impl2(c: Context)(x: c.Tree) = {
    import c.universe._
    q"new ${symbolOf[RichC]}"
  }

}
