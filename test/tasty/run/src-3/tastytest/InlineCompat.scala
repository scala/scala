package tastytest

import scala.language.experimental.macros

import scala.reflect.macros.blackbox.Context

import scala.annotation.experimental

object InlineCompat {

  @experimental
  def foo(code: String): String = macro InlineCompatScala2Macro.foo

  @experimental
  inline def foo(inline code: String): String = code // inline method, not macro

}

object InlineCompatScala2Macro {
  def foo(c: Context)(code: c.Tree): c.Tree = code
}
