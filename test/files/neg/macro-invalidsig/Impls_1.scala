import scala.reflect.runtime.universe._
import scala.reflect.macros.Context

object Impls1 {
  def foo[U: c.WeakTypeTag: Numeric](c: Context) = {
    import c.universe._
    Literal(Constant(42))
  }
}

object Impls2 {
  def foo = ???
}

object Impls3 {
  def foo(c: scala.reflect.api.Universe) = ???
}

object Impls4 {
  def foo(cs: Context*) = ???
}

object Impls5 {
  def foo(c: Context) = ???
}

object Impls6 {
  def foo[T, U: c.WeakTypeTag](c: Context)(implicit x: c.Expr[Int]) = {
    import c.{prefix => prefix}
    import c.universe._
    val body = Block(List(
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant("invoking foo_targs...")))),
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant("type of prefix is: " + prefix.staticType)))),
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant("U is: " + implicitly[c.WeakTypeTag[U]].tpe))))),
      Literal(Constant(())))
    c.Expr[Unit](body)
  }
}

object Impls7 {
  def foo(c: Context)(x: c.Expr[Int], y: c.Expr[Int]) = ???
}

object Impls8 {
  def foo(c: Context)(x: c.universe.Symbol) = ???
}

object Impls9 {
  def foo(c: Context)(xs: c.Expr[Int]*) = ???
}

object Impls10 {
  def foo(c: Context)(y: c.Expr[Int], x: c.Expr[Int]) = ???
}

object Impls11 {
  def foo[U](c: Context)(U: c.universe.Type) = ???
}

object Impls12 {
  def foo[U <: String](c: Context) = ???
}

object Impls13 {
  def foo[U <: String](c: Context) = ???
}

object Impls14 {
  def foo[U: c.WeakTypeTag](c: Context) = ???
}

object Impls15 {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag, V](c: Context)(implicit V: c.WeakTypeTag[V]): c.Expr[Unit] = {
    println(implicitly[c.WeakTypeTag[T]])
    println(implicitly[c.WeakTypeTag[U]])
    println(V)
    c.literalUnit
  }
}

object Impls16 {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag, V](c: Context)(implicit V: c.WeakTypeTag[V]): c.Expr[Unit] = {
    println(implicitly[c.WeakTypeTag[T]])
    println(implicitly[c.WeakTypeTag[U]])
    println(V)
    c.literalUnit
  }
}