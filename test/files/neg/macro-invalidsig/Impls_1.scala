import scala.reflect.runtime.universe._
import scala.reflect.macros.BlackboxContext

object Impls1 {
  def foo[U: c.WeakTypeTag: Numeric](c: BlackboxContext) = { import c.universe._; q"42" }
}

object Impls2 {
  def foo = ???
}

object Impls3 {
  def foo(c: scala.reflect.api.Universe) = ???
}

object Impls4 {
  def foo(cs: BlackboxContext*) = ???
}

object Impls5 {
  def foo(c: BlackboxContext) = ???
}

object Impls6 {
  def foo[T, U: c.WeakTypeTag](c: BlackboxContext)(implicit x: c.Expr[Int]) = {
    import c.{prefix => prefix}
    import c.universe._
    c.Expr[Unit](q"""
      println("invoking foo_targs...")
      println("type of prefix is: " + ${prefix.staticType.toString})
      println("U is: " + ${implicitly[c.WeakTypeTag[U]].tpe.toString})
    """)
  }
}

object Impls7 {
  def foo(c: BlackboxContext)(x: c.Expr[Int], y: c.Expr[Int]) = ???
}

object Impls8 {
  def foo(c: BlackboxContext)(x: c.universe.Symbol) = ???
}

object Impls9 {
  def foo(c: BlackboxContext)(xs: c.Expr[Int]*) = ???
}

object Impls10 {
  def foo(c: BlackboxContext)(y: c.Expr[Int], x: c.Expr[Int]) = ???
}

object Impls11 {
  def foo[U](c: BlackboxContext)(U: c.universe.Type) = ???
}

object Impls12 {
  def foo[U <: String](c: BlackboxContext) = ???
}

object Impls13 {
  def foo[U <: String](c: BlackboxContext) = ???
}

object Impls14 {
  def foo[U: c.WeakTypeTag](c: BlackboxContext) = ???
}

object Impls15 {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag, V](c: BlackboxContext)(implicit V: c.WeakTypeTag[V]): c.Expr[Unit] = {
    import c.universe._
    println(implicitly[c.WeakTypeTag[T]])
    println(implicitly[c.WeakTypeTag[U]])
    println(V)
    c.Expr[Unit](q"()")
  }
}

object Impls16 {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag, V](c: BlackboxContext)(implicit V: c.WeakTypeTag[V]): c.Expr[Unit] = {
    import c.universe._
    println(implicitly[c.WeakTypeTag[T]])
    println(implicitly[c.WeakTypeTag[U]])
    println(V)
    c.Expr[Unit](q"()")
  }
}