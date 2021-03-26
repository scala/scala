package tastytest

import scala.language.experimental.macros

import scala.reflect.macros.blackbox.Context

object InlineCompat {

  def foo(code: String): String = macro InlineCompatScala2Macro.foo
  inline def foo(inline code: String): String = code // inline method, not macro

}

object InlineCompatScala2Macro {
  def foo(c: Context)(code: c.Tree): c.Tree = code
}
