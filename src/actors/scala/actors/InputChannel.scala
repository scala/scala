package scala.actors

trait InputChannel[Msg] {
  def receive[R](f: PartialFunction[Msg, R]): R
  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R
  def react(f: PartialFunction[Any, Unit]): Nothing
  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing
}
