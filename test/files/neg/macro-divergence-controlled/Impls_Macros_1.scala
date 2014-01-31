import scala.reflect.macros.whitebox.Context
import language.experimental.macros

trait Complex[T]

class Foo(val foo: Foo)

object Complex {
  def impl[T: c.WeakTypeTag](c: Context): c.Expr[Complex[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    for (f <- tpe.decls.collect{case f: TermSymbol if f.isParamAccessor && !f.isMethod => f}) {
      val trecur = appliedType(typeOf[Complex[_]], List(f.info))
      if (c.openImplicits.tail.exists(ic => ic.pt =:= trecur)) c.abort(c.enclosingPosition, "diverging implicit expansion. reported by a macro!")
      val recur = c.inferImplicitValue(trecur, silent = true)
      if (recur == EmptyTree) c.abort(c.enclosingPosition, s"couldn't synthesize $trecur")
    }
    c.Expr[Null](q"null")
  }

  implicit object ComplexString extends Complex[String]
  implicit def genComplex[T]: Complex[T] = macro impl[T]
}
