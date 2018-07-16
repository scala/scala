import scala.language.experimental.macros

final class Ops[T](val x: T) extends AnyVal {
  def f: Unit = macro Macros.crash
}
