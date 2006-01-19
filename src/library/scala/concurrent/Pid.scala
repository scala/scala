/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent;

class Pid(actor: Actor) {
  var target = actor;

  def !(msg: MailBox#Message) = target send msg;

  def spawn(body: Actor => Unit): Pid = {
    val a = new Actor {
      override def run: Unit = body(this);
    };
    a.start;
    a.self
  }

  def spawnReceive(cases: PartialFunction[MailBox#Message,Unit]) = {
    val a = new Actor {
      override def run: Unit = receive(cases);
    };
    a.start;
    a.self
  }

  override def hashCode() = target.hashCode();

  override def equals(that: Any) =
    if (this.hashCode() == that.hashCode()) true;
    else false;

  override def toString() = "Pid(" + target + ")";
}
