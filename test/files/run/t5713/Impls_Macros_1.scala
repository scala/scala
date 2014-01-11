package m

import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Level extends Enumeration {
  val Error = Value(5)
}

object Logger {
  def error(message: String): Unit = macro LoggerMacros.error
}

private object LoggerMacros {

  type LoggerContext = Context { type PrefixType = Logger.type }

  def error(c: LoggerContext)(message: c.Expr[String]): c.Expr[Unit] =
    log(c)(c.universe.reify(Level.Error), message)

  private def log(c: LoggerContext)(level: c.Expr[Level.Value], message: c.Expr[String]): c.Expr[Unit] =
// was:    if (level.splice.id < 4) // TODO Remove hack!
    if (c.eval(level).id < 4) // TODO Remove hack!
      c.universe.reify(())
    else {
      c.universe.reify(println(message.splice))
    }
}