package tastytest

import scala.language.experimental.macros

import scala.reflect.macros.blackbox.Context

object InlineCompat2 {

  def foo(code: String): String = macro InnerScala2MacroImpl.fooImpl
  inline def foo(inline code: String): String = code // inline method, not macro

  object InnerScala2MacroImpl {
    def fooImpl(c: Context)(code: c.Tree): c.Tree = code
  }

}
