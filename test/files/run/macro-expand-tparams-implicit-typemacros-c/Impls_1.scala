import scala.reflect.macros.{Context => Ctx}
import scala.reflect.runtime.universe._

class C[T: TypeTag](x: T) { def tpe = typeOf[T] }

object Impls {
  def impl[T: c.WeakTypeTag](c: Ctx)(x: c.Expr[T]) = {
    import c.universe._
    Apply(Ident(TypeName("C")), List(x.tree))
  }

  def implImpl[T: c.WeakTypeTag](c: Ctx)(x: c.Expr[T]) = {
    import c.universe._
    Apply(Select(Ident(TermName("Macros")), TypeName("Foo")), List(x.tree))
  }
}