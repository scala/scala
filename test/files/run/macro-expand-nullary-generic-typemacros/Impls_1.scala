import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

class C(val msg: String)
object Impls {
  def impl[T: c.WeakTypeTag](c: Ctx)(meth: String) = {
    import c.universe._
    Apply(Ident(TypeName("C")), List(Literal(Constant(s"$meth[${c.weakTypeOf[T]}]"))))
  }

  def fooNullary[T: c.WeakTypeTag](c: Ctx) = impl[T](c)("fooNullary")
  def fooEmpty[T: c.WeakTypeTag](c: Ctx)() = impl[T](c)("fooEmpty")
  def barNullary[T: c.WeakTypeTag](c: Ctx)(x: c.Expr[Int]) = impl[T](c)("barNullary")
  def barEmpty[T: c.WeakTypeTag](c: Ctx)(x: c.Expr[Int])() = impl[T](c)("barEmpty")
}