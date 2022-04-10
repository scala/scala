
import language.experimental.macros

import reflect.macros._

package object test {
  def wrapper(expr: Any): List[Any] = macro TestMacros.wrapper_impl
}

package test {

  object TestMacros {

    def wrapper_impl(c: blackbox.Context)(expr: c.Expr[Any]): c.Expr[List[Any]] = {
      import c.universe._

      val f = q"(x: Any) => List($expr)"
      val g = c.typecheck(f)
      c.universe.internal.changeOwner(expr.tree, c.internal.enclosingOwner, g.symbol)
      val code = q"List.empty[Any].flatMap($g)"

      // see doc at macros.Universe#MacroInternalApi#changeOwner
      //val code = q"List.empty[Any].flatMap(x => List($expr))"

      c.Expr[List[Any]](code)
    }
  }
}
