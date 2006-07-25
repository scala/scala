/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Process.scala 7931 2006-06-20 16:34:51Z odersky $


package scala.actors.threads;

/**
 * The object <code>Process</code> ...
 *
 * @author Martin Odersky
 * @version 1.0
 */
object Process {

  def spawn(body: => Unit): Process = {
    val p = new Process {
      override def run(): Unit = body
    }
    p.start();
    p
  }

  def spawnLink(body: => Unit): Process = {
    val p = new Process {
      override def run(): Unit = body
    }
    self.link(p)
    p.start()
    p
  }

  def send(p: Process, msg: Any) =
    p ! msg

  def receive[a](f: PartialFunction[Any, a]): a =
    self.receive(f);

  def receiveWithin[a](msec: long)(f: PartialFunction[Any, a]): a =
    self.receiveWithin(msec)(f);

  def self: Process =
    if (Thread.currentThread().isInstanceOf[Process])
      Thread.currentThread().asInstanceOf[Process]
    else
      error("Self called outside a process");

  def exit(p: Process, reason: Symbol) =
    p.exit(reason);

}

/**
 * The class <code>Process</code> ...
 *
 * @author Martin Odersky
 * @version 1.0
 */
class Process extends Actor[Any] with scala.actors.Process {
  private var exitReason: AnyRef = null;
  private var links: List[scala.actors.Process] = Nil;

  override def start(): Unit = try { run() }
  catch {
    case _: java.lang.InterruptedException =>
      signal(exitReason)
    case (exitSignal) =>
      signal(exitSignal)
  }

  private def signal(s: Any) =
    links.foreach { p: scala.actors.Process => p ! Triple('EXIT, this, s) }

  def link(p: scala.actors.Process): Unit =
    links = p::links;

  def linkTo(to: scala.actors.Process): Unit =
    links = to::links;

  // TODO
  def unlink(from: scala.actors.Process): Unit = {}

  // TODO
  def unlinkFrom(from: scala.actors.Process): Unit = {}

  def exit(reason: Symbol): Unit = {
    exitReason = reason;
    interrupt()
  }

  def exit(from: scala.actors.Process, reason: Symbol): Unit =
    from.exit(reason)
}
