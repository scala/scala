package examples;

import scala.concurrent._;

class OnePlaceBuffer() {
  private val m = new MailBox();             // An internal mailbox
  private case class Empty(), Full(x: Int);  // Types of messages we deal with

  m send Empty();                            // Initialization

  def write(x: Int): Unit =
    m receive { case Empty() => m send Full(x) }

  def read: Int =
    m receive { case Full(x) => m send Empty() ; x }
}
