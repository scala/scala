package scala.tools.nsc.interactive

import java.io.{Reader, Writer}

import Pickler._
import Lexer.EOF

abstract class LogReplay {
  def logreplay(event: String, x: => Boolean): Boolean
  def logreplay[T: Pickler](event: String, x: => Option[T]): Option[T]
  def close()
  def flush()
}

class Logger(wr0: Writer) extends LogReplay {
  val wr = new PrettyWriter(wr0)
  private var first = true
  private def insertComma() = if (first) first = false else wr.write(",")

  def logreplay(event: String, x: => Boolean) = {
    val xx = x
    if (xx) { insertComma(); pkl[Unit].labelled(event).pickle(wr, ()) }
    xx
  }
  def logreplay[T: Pickler](event: String, x: => Option[T]) = {
    val xx = x
    xx match {
      case Some(y) => insertComma(); pkl[T].labelled(event).pickle(wr, y)
      case None =>
    }
    xx
  }
  def close() { wr.close() }
  def flush() { wr.flush() }
}

object NullLogger extends LogReplay {
  def logreplay(event: String, x: => Boolean) = x
  def logreplay[T: Pickler](event: String, x: => Option[T]) = x
  def close() {}
  def flush() {}
}

class Replayer(raw: Reader) extends LogReplay {
  private val rd = new Lexer(raw)
  private var nextComma = false

  private def eatComma() =
    if (nextComma) { rd.accept(','); nextComma = false }

  def logreplay(event: String, x: => Boolean) =
    if (rd.token == EOF) NullLogger.logreplay(event, x)
    else {
      eatComma()
      pkl[Unit].labelled(event).unpickle(rd) match {
        case UnpickleSuccess(_) => nextComma = true; true
        case _ => false
      }
    }

  def logreplay[T: Pickler](event: String, x: => Option[T]) =
    if (rd.token == EOF) NullLogger.logreplay(event, x)
    else {
      eatComma()
      pkl[T].labelled(event).unpickle(rd) match {
        case UnpickleSuccess(y) => nextComma = true; Some(y)
        case _ => None
      }
    }

  def close() { raw.close() }
  def flush() {}
}

