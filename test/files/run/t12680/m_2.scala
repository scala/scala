
import language.experimental.macros
import scala.reflect.macros._

object M {
  def f(clazz: Class[_], j: J_1): String = macro g

  def g(c: blackbox.Context)(clazz: c.Tree, j: c.Tree): c.Tree = {
    import c.universe._
    val classValue = c.eval(c.Expr[Class[_]](c.untypecheck(clazz)))
    val jValue = c.eval(c.Expr[J_1](c.untypecheck(j)))

    Literal(Constant(s"$classValue, $jValue"))
  }
}
