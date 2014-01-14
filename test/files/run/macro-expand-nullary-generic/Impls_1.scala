import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context

object Impls {
  def impl[T: c.WeakTypeTag](c: Context)(meth: String) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(s"$meth[${c.weakTypeOf[T]}]"))))
    c.Expr[Unit](body)
  }

  def fooNullary[T: c.WeakTypeTag](c: Context) = impl[T](c)("fooNullary")
  def fooEmpty[T: c.WeakTypeTag](c: Context)() = impl[T](c)("fooEmpty")
  def barNullary[T: c.WeakTypeTag](c: Context)(x: c.Expr[Int]) = impl[T](c)("barNullary")
  def barEmpty[T: c.WeakTypeTag](c: Context)(x: c.Expr[Int])() = impl[T](c)("barEmpty")
}