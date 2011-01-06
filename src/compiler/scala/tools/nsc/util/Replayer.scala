package scala.tools.nsc.util

import Pickler.{PickleIterator, valueIterator, Result, Success}
import scala.tools.nsc.io.JSON._

abstract class LogReplay {
  def logreplay(event: String, x: => Boolean): Boolean
  def logreplay[T](event: String, x: => Option[T])(implicit pt: Pickler[T]): Option[T]
}

class Logger(wr: java.io.Writer) extends LogReplay {
  wr.write('{')
  def logreplay(event: String, x: => Boolean) = {
    if (x) (event J_: JNull) write wr
    x
  }
  def logreplay[T](event: String, x: => Option[T])(implicit pt: Pickler[T]) = {
    x match {
      case Some(y) => (event J_: (pt pickle y)) write wr
      case None =>
    }
    x
  }
  def close() {
    wr.write('}')
    wr.close()
  }
}

object NullLogger extends LogReplay {
  def logreplay(event: String, x: => Boolean) = x
  def logreplay[T](event: String, x: => Option[T])(implicit pt: Pickler[T]) = x
  def close() {}
}

class Replayer(log: java.io.Reader) extends LogReplay {
  var rdr = new JReader(log)
  val it: BufferedIterator[J_:] = rdr.nextValue() match {
    case jo: JObject =>
      jo.inputIterator
    case _ =>
      throw new MalformedInput(rdr, "no top-level object found")
  }
  def logreplay(event: String, x: => Boolean) = it.head match {
    case `event` J_: _ => it.next(); true
    case _ => false
  }
  def logreplay[T](event: String, x: => Option[T])(implicit pt: Pickler[T]) = it.head match {
    case `event` J_: rhs =>
      it.next()
      val Success(result) = pt.unpickle(valueIterator(rhs))
      Some(result)
    case _ => None
  }
  def close() {
    log.close()
  }
}

