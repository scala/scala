import scala.language.experimental.macros
import scala.reflect.macros.Macro
import scala.reflect.macros.Context

trait Bundle[T] extends Macro {
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
  def foo = macro Bundle[Int].impl
  def foo = macro Bundle[Int, Nothing].impl
}