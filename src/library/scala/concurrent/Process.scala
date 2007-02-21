/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent


/** This object ...
 *
 *  @author  Erik Stenman
 *  @version 1.0, 01/10/2003
 */
object Process {

  def spawn(body: => Unit): Process = {
    val p = new Process(body)
    p.start()
    p
  }

  def spawn_link(body: => Unit): Process =
    self.spawn_link(body)

  def send(p: Process, msg: MailBox#Message) =
    p.send(msg)

  def receive[a](f: PartialFunction[MailBox#Message, a]): a =
    self.receive(f)

  def receiveWithin[a](msec: long)(f: PartialFunction[MailBox#Message, a]): a =
    self.receiveWithin(msec)(f)

  /**
   *  @return the self process
   *  @throws Predef.UnsupportedOperationException if self called outside a process.
   */
  def self: Process =
    if (currentThread.isInstanceOf[Process])
      currentThread.asInstanceOf[Process]
    else
      throw new UnsupportedOperationException("Self called outside a process")

  def exit(p: Process, reason: AnyRef) =
    p.exit(reason)

}

class Process(body: => Unit) extends Actor() {
  private var exitReason: AnyRef = null
  private var links: List[Process] = Nil

  override def run() =
    try {
      body
    }
    catch {
      case _: java.lang.InterruptedException =>
        signal(exitReason)
      case (exitSignal) =>
        signal(exitSignal)
    }

  private def signal(s: MailBox#Message) =
    links.foreach { p: Process => p.send(('EXIT, this, s)) }

  def !(msg: MailBox#Message) =
    send(msg)

  def link(p: Process) =
    links = p::links

  def spawn_link(body: => Unit) = {
    val p = new Process(body)
    p.link(this)
    p.start()
    p
  }

  //def self = this

  def exit(reason: AnyRef): Unit = {
    exitReason = reason
    interrupt()
  }

}
