// using scala 2.13.17
// using dep org.scala-lang:scala-compiler:2.13.17

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Stub {
  def stub[A](impl: A): Unit = macro stub0[A]

  def stub0[A: c.WeakTypeTag](c: Context)(impl: c.Expr[A]): c.Tree =
    c.untypecheck(impl.tree)
}
