import scala.language.experimental.macros
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}

class Bundle(val c: BlackboxContext) {
  def this(c: WhiteboxContext) = this(c: BlackboxContext)
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
}