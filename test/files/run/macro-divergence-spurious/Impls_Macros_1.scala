import scala.reflect.macros.Context
import language.experimental.macros

trait Complex[T]

class Foo(val bar: Bar)
class Bar(val s: String)

object Complex {
  def impl[T: c.WeakTypeTag](c: Context): c.Expr[Complex[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    for (f <- tpe.declarations.collect{case f: TermSymbol if f.isParamAccessor && !f.isMethod => f}) {
      val trecur = appliedType(typeOf[Complex[_]], List(f.typeSignature))
      val recur = c.inferImplicitValue(trecur, silent = true)
      if (recur == EmptyTree) c.abort(c.enclosingPosition, s"couldn't synthesize $trecur")
    }
    c.literalNull
  }

  implicit object ComplexString extends Complex[String]
  implicit def genComplex[T]: Complex[T] = macro impl[T]
}
