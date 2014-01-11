import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}

class Logger {
  def error1(message: String) = macro Impls.error1
  def error2(message: String) = macro Impls.error2
}

object Impls {
  type LoggerContext1 = BlackboxContext { type PrefixType = Logger }
  def error1(c: LoggerContext1)(message: c.Expr[String]): c.Expr[Unit] = ???

  type LoggerContext2 = WhiteboxContext { type PrefixType = Logger }
  def error2(c: LoggerContext2)(message: c.Expr[String]): c.Expr[Unit] = ???
}
