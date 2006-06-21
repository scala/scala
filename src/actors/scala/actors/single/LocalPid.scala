package scala.actors.single;

import scala.collection.mutable.Queue;

/**
 * @author Philipp Haller
 */
class LocalPid(actor: Actor) extends Pid {
  var target = actor

  def !(msg: MailBox#Message): unit = target send msg

  def become(clos: Actor => Unit) = {
    // old actor should become anonymous (cannot receive any messages any more)
    // achieved by removing it from target of pid.

    val oldActor = target
    //Debug.info("old actor: " + oldActor);

    // change our target to point to a newly created actor with the same mailbox.

    val newActor = new Actor {
      override def run: Unit = clos(this)
    }
    newActor.sent = oldActor.sent
    target = newActor
    newActor.self = this

    //Debug.info("new actor: " + newActor);

    // clean mailbox of now anonymous actor (must not receive any messages any more; pending messages are for new actor)
    oldActor.sent = new Queue[MailBox#Message]

    //Debug.info("Starting new actor.");
    newActor.start  // important to start after changing pid because actor may send messages to itself.
  }

  private class ProxyPartialFunction(a: Actor, f: PartialFunction[MailBox#Message,unit]) extends PartialFunction[MailBox#Message, unit] {
    def isDefinedAt(m: MailBox#Message): boolean = f.isDefinedAt(m)
    def apply(m: MailBox#Message): unit = {
      f(m)
      a receive this
    }
  }

  def becomeReceiveLoop(f: PartialFunction[MailBox#Message,unit]) = {

    become(a => a receive new ProxyPartialFunction(a, f))

    /*become(
      a:Actor => {
        def loop: unit = {
          def proxyFun(m: MailBox#Message): unit = {
            if (f.isDefinedAt(m)) {
              f(m);
              loop
            }
          };
          //a receive proxyFun
        }
        loop
      }
    )*/
  }

  def spawn(body: Actor => Unit): Pid = {
    val a = new Actor {
      override def run: Unit = body(this)
    }
    a.start
    a.self
  }

  def spawnReceive(cases: PartialFunction[MailBox#Message,Unit]) = {
    val a = new Actor {
      override def run: Unit = receive(cases)
    }
    a.start
    a.self
  }

  override def toString() = "LocalPid(" + target + ")"
}
