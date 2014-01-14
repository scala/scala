import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context

object Impls {
  def foo_targs[T, U: c.WeakTypeTag](c: Context = null)(x: c.Expr[Int] = null) = {
    import c.{prefix => prefix}
    import c.universe._
    val U = implicitly[c.WeakTypeTag[U]]
    c.Expr[Unit](q"""
      println("invoking foo_targs...")
      println("type of prefix is: " + ${prefix.staticType.toString})
      println("type of prefix tree is: " + ${prefix.tree.tpe.toString})
      println("U is: " + ${U.tpe.toString})
    """)
  }
}

class Macros[T] {
  def foo_targs[U](x: Int) = macro Impls.foo_targs[T, U]
}