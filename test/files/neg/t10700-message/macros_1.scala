
package scala
package testing

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def m[A]: String = macro Impls.mImpl[A]
}

object Impls {
  def mImpl[A : c.WeakTypeTag](c: Context): c.Expr[String] = {
    import c._
    val g = universe.asInstanceOf[scala.tools.nsc.Global]
    import g.typer.infer.InferErrorGen._
    val t = implicitly[c.WeakTypeTag[A]].tpe.asInstanceOf[g.analyzer.global.Type]
    val msg = NotWithinBoundsErrorMessage(prefix = "test ", targs = List(t), tparams = Nil, explaintypes = false)
    abort(macroApplication.pos, msg)
    Expr[String] {
      import universe._
      Literal(Constant(msg))
    }
  }
}
// was: java.util.NoSuchElementException: head of empty list
