import scala.reflect.macros.Context

class Logger {
  def error(message: String) = macro Impls.error
}

object Impls {
  type LoggerContext = Context { type PrefixType = Logger }
  def error(c: LoggerContext)(message: c.Expr[String]): c.Expr[Unit] = ???
}
