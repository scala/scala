import language.experimental.macros
import scala.reflect.macros.blackbox.Context

class C1(val n: Int) extends AnyVal
class C2(val n: Int) extends AnyRef

object Macros {
  type F1 = C1
  type F2 = C2

  def foo = macro impl
  def impl(c: Context) = {
    import c.universe._
    def test[T: c.TypeTag] = reify(println(c.Expr[String](Literal(Constant(c.reifyRuntimeClass(c.typeOf[T]).toString))).splice)).tree
    def tests = Block(List(test[C1], test[C2], test[F1], test[F2]), Literal(Constant(())))
    c.Expr[Unit](tests)
  }
}