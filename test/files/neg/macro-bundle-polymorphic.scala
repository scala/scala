import scala.language.experimental.macros
import scala.reflect.macros.BlackboxMacro

trait Bundle[T] extends BlackboxMacro {
  def impl = ???
}

object Macros {
  def foo = macro Bundle.impl
  def foo = macro Bundle[Int].impl
  def foo = macro Bundle[Int, Nothing].impl
}