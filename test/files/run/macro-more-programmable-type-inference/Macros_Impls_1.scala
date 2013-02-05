import scala.reflect.macros.{Context, Macro}
import scala.language.experimental.macros

class Foo[T]
object Foo {
  implicit def foo[T](i: Int): Foo[T] = macro Impl.impl[T]
}

trait Impl extends Macro {
  def impl[T: c.WeakTypeTag](i: c.Expr[Int]): c.Expr[Foo[T]] = {
    import c.universe._
    val tpe = AppliedTypeTree(Ident(TypeName("Foo")), List(TypeTree(weakTypeOf[T])))
    c.Expr[Foo[T]](Apply(Select(New(tpe), nme.CONSTRUCTOR), List()))
  }

  override def onInfer(tic: c.TypeInferenceContext): Unit = {
    import c.universe._
    val Apply(_, List(Literal(Constant(i: Int)))) = tic.tree
    val T = tic.unknowns(0)
    tic.infer(T, if (i == 0) typeOf[Int] else typeOf[String])
  }
}