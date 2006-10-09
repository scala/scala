/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

case object TIMEOUT

class SuspendActorException extends Throwable {
  /*
   * For efficiency reasons we do not fill in
   * the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = {
    this
  }
}

/**
 * This class provides a means for typed communication among
 * actors. Only the actor creating an instance of a
 * <code>Channel</code> may receive from it.
 *
 * @author Philipp Haller
 */
class Channel[Msg] extends InputChannel[Msg] with OutputChannel[Msg] {

  private[actors] var receiver: Actor = synchronized {
    // basically Actor.self, but can be null
    val t = Thread.currentThread()
    if (t.isInstanceOf[ActorThread])
      t.asInstanceOf[ActorThread]
    else {
      val a = Actor.selfs.get(t).asInstanceOf[Actor]
      a
    }
  }

  private var received: Msg = _

  private val waitingForNone = (m: Msg) => false
  private var waitingFor: Msg => boolean = waitingForNone
  private var waitingForSender: Actor = null

  private val messageQueue = new MessageQueue[Msg]

  private def send(msg: Msg, sender: Actor) = receiver.synchronized {
    receiver.tick()
    if (waitingFor(msg) && ((waitingForSender == null) ||
        (waitingForSender == sender))) {
      received = msg
      receiver.pushSender(sender)
      waitingFor = waitingForNone
      waitingForSender = null

      if (receiver.isInstanceOf[Reactor]) {
        val myReactor = receiver.asInstanceOf[Reactor]
        if (myReactor.timeoutPending) {
          myReactor.timeoutPending = false
          TimerThread.trashRequest(myReactor)
        }
      }

      receiver.scheduleActor(null, msg)
    } else {
      messageQueue.append(msg, sender)
    }
  }

  /**
   * Sends <code>msg</code> to this <code>Channel</code>.
   */
  def !(msg: Msg): unit = send(msg, Actor.self)

  /**
   * Sends <code>msg</code> to this <code>Channel</code> and
   * awaits reply.
   */
  def !?(msg: Msg): Any = {
    Actor.self.freshReply()
    this ! msg
    Actor.self.reply.receiveFrom(receiver) {
      case x => x
    }
  }

  /**
   * Forwards <code>msg</code> to <code>this</code> keeping the
   * last sender as sender instead of <code>self</code>.
   */
  def forward(msg: Msg): unit = send(msg, receiver.sender)

  /**
   * Receives a message from this <code>Channel</code>.
   */
  def receive[R](f: PartialFunction[Msg, R]): R = {
    assert(Actor.self == receiver, "receive from channel belonging to other actor")
    assert(receiver.isThreaded, "receive invoked from reactor")
    receiver.synchronized {
      receiver.tick()
      waitingFor = f.isDefinedAt
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
      }
      // acquire lock because we might call wait()
      else synchronized {
        receiver.suspendActor()
      }
      waitingFor = waitingForNone
    }
    receiver.resetActor()
    val result = f(received)
    receiver.popSender()
    result
  }

  private[actors] def receiveFrom[R](r: Actor)(f: PartialFunction[Msg, R]): R = {
    assert(Actor.self == receiver, "receive from channel belonging to other actor")
    assert(receiver.isThreaded, "receive invoked from reactor")
    receiver.synchronized {
      receiver.tick()
      waitingFor = f.isDefinedAt
      waitingForSender = r
      var q = messageQueue.dequeueFirst((item: MessageQueueResult[Msg]) => {
        waitingFor(item.msg) && item.sender == r
      })
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
      }
      else synchronized {
        receiver.suspendActor()
      }
      waitingFor = waitingForNone
      waitingForSender = null
    }
    receiver.resetActor()
    val result = f(received)
    receiver.popSender()
    result
  }

  /**
   * Receives a message from this <code>Channel</code>. If no
   * message could be received before <code>msec</code>
   * milliseconds elapsed, the <code>TIMEOUT</code> action is
   * executed if specified.
   */
  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R = {
    assert(Actor.self == receiver, "receive from channel belonging to other actor")
    assert(receiver.isThreaded, "receive invoked from reactor")
    receiver.synchronized {
      receiver.tick()
      waitingFor = f.isDefinedAt
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
      }
      else synchronized {
        receiver.suspendActorFor(msec)
        if (received == null)
          if (f.isDefinedAt(TIMEOUT)) {
            receiver.resetActor()
            val result = f(TIMEOUT)
            return result
          }
          else
            error("unhandled timeout")
      }
      waitingFor = waitingForNone
    }
    receiver.resetActor()
    val result = f(received)
    receiver.popSender()
    result
  }

  /**
   * <code>receive</code> for reactors.
   */
  def react(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.self == receiver, "react on channel belonging to other actor")
    receiver.synchronized {
      receiver.tick()
      waitingFor = f.isDefinedAt
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
        waitingFor = waitingForNone
        receiver.scheduleActor(f, received)
      }
      else synchronized {
        receiver.detachActor(f)
      }
      throw new SuspendActorException
    }
  }

  /**
   * <code>receiveWithin</code> for reactors.
   */
  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing = {
    assert(Actor.self == receiver, "react on channel belonging to other actor")
    receiver.synchronized {
      receiver.tick()
      waitingFor = f.isDefinedAt
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
        waitingFor = waitingForNone
        receiver.scheduleActor(f, received)
      }
      else synchronized {
        TimerThread.requestTimeout(receiver.asInstanceOf[Reactor], f, msec)
        receiver.asInstanceOf[Reactor].timeoutPending = true
        receiver.detachActor(f)
      }
      throw new SuspendActorException
    }
  }
}
