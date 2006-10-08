/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala.actors

import scala.collection.mutable.HashSet

/**
 * This object provides functions for the definition of actors and
 * reactors, as well as all actor operations, such as
 * <code>receive</code>, <code>react</code>, <code>reply</code>,
 * etc.
 *
 * @author Philipp Haller
 */
object Actor {

  private[actors] val selfs = new java.util.WeakHashMap(16, 0.5f)

  /**
   * Returns the currently executing actor. Should be used instead
   * of <code>this</code> in all blocks of code executed by
   * actors.
   */
  def self: Actor = synchronized {
    val t = Thread.currentThread()
    if (t.isInstanceOf[ActorThread])
      t.asInstanceOf[ActorThread]
    else {
      var a = selfs.get(t).asInstanceOf[Actor]
      if (a == null) {
        a = new ActorProxy(t)
        selfs.put(t, a)
      }
      a
    }
  }

  /**
   * Creates an instance of a thread-based actor executing <code>body</code>,
   * and starts it.
   */
  def actor(body: => Unit): ActorThread = synchronized {
    val actor = new ActorThread {
      def act() = body
    }
    actor.start()
    actor
  }

  /**
   * Creates an instance of a thread-based actor specifying a
   * channel which can be used for typed communication with other
   * actors.
   */
  def actor[a](ch: Channel[a])(body: => Unit): ActorThread = synchronized {
    val actor = new ActorThread {
      def act() = body
    }
    ch.receiver = actor
    actor.start()
    actor
  }

  /**
   * Creates an instance of an event-based reactor executing
   * <code>body</code>, and starts it.
   */
  def reactor(body: => Unit): Reactor = synchronized {
    val reactor = new Reactor {
      def act() = body
    }
    reactor.start()
    reactor
  }

  /**
   * Receives a message from the mailbox of
   * <code>self</code>. Blocks if no message matching any of the
   * cases of <code>f</code> can be received.
   *
   * Only (thread-based) actors may call this method. It fails at
   * runtime if executed by a reactor.
   */
  def receive[a](f: PartialFunction[Any, a]): a =
    self.in.receive(f)

  /**
   Receives a message from the mailbox of
   <code>self</code>. Blocks at most <code>msec</code>
   milliseconds if no message matching any of the cases of
   <code>f</code> can be received. If no message could be
   received the <code>TIMEOUT</code> action is executed if
   specified.

   Only (thread-based) actors may call this method. It fails at
   runtime if executed by a reactor.
   */
  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R =
    self.in.receiveWithin(msec)(f)

  /**
   <code>receive</code> for event-based reactors.

   Actions in <code>f</code> have to contain the rest of the
   computation of <code>self</code>, as this method will never
   return.
   */
  def react(f: PartialFunction[Any, Unit]): Nothing =
    self.in.react(f)

  /**
   <code>receiveWithin</code> for event-based reactors.

   Actions in <code>f</code> have to contain the rest of the
   computation of <code>self</code>, as this method will never
   return.
   */

  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing =
    self.in.reactWithin(msec)(f)

  /*
  def eventloop(f: PartialFunction[Any, Unit]): Nothing =
    self.in.react(new RecursiveProxyHandler(self, f))

  private class RecursiveProxyHandler(a: Actor, f: PartialFunction[Any, Unit])
          extends PartialFunction[Any, Unit] {
    def isDefinedAt(m: Any): boolean =
      true // events are immediately removed from the mailbox
    def apply(m: Any): Unit = {
      if (f.isDefinedAt(m)) f(m)
      self.in.react(this)
    }
  }
  */

  /**
   Used for receiving a message from a specific actor.
   Example: <code>from (a) receive { //... }</code>
   */
  def from(r: Actor): FromReceive =
    new FromReceive(r)

  private[actors] class FromReceive(r: Actor) {
    def receive[a](f: PartialFunction[Any, a]): a =
      self.in.receiveFrom(r)(f)
  }

  /**
   Returns the actor which sent the last received message.
   */
  def sender: Actor = self.sender

  /**
   Send <code>msg</code> to the actor waiting in a call to
   <code>!?</code>.
   */
  def reply(msg: Any): Unit = sender.reply ! msg

  /**
   Send <code>()</code> to the actor waiting in a call to
   <code>!?</code>.
   */
  def reply(): Unit = reply(())

  private[actors] trait Body[T] {
    def orElse(other: => T): T
    def andThen(other: => T): T
  }

  implicit def mkBody(body: => Unit) = new Body[Unit] {
    def orElse(other: => Unit): Unit = choose(body, other)
    def andThen(other: => Unit): Unit = seq(body, other)
  }

  private[actors] def choose(alt1: => Unit, alt2: => Unit): Unit = {
    val s = self
    // save former custom suspendActor function
    // (e.g. from further orElse)
    val suspendNext = s.suspendActor
    val detachNext = s.detachActor

    // have to get out of the point of suspend in alt1's
    // receive
    s.suspendActor = () => { throw new SuspendActorException }
    s.detachActor = f => { throw new SuspendActorException }

    try { alt1 } catch {
      case d: SuspendActorException => {
        s.suspendActor = suspendNext
        s.detachActor = detachNext
        alt2
      }
    }
  }

  /**
   Causes <code>self</code> to repeatedly execute
   <code>body</code>.
   */
  def loop(body: => Unit): Unit = {
    val s = self
    s.kill = () => { body; s.kill() }
    body
  }

  /**
   Causes <code>self</code> to execute <code>first</code>
   followed by <code>next</code>.
   */
  def seq(first: => Unit, next: => Unit): Unit = {
    val s = self
    s.kill = () => { next }
    first
  }

  /**
   Links <code>self</code> to actor <code>to</code>.
   */
  def link(to: Actor): Actor = self.link(to)

  /**
   Links <code>self</code> to actor defined by <code>body</code>.
   */
  def link(body: => Unit): Actor = self.link(body)

  /**
   Unlinks <code>self</code> from actor <code>from</code>.
   */
  def unlink(from: Actor): Unit = self.unlink(from)

  /**
   Terminates execution of <code>self</code> with the following
   effect on linked actors:

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>true</code>, send message
   <code>Exit(self, reason)</code> to <code>a</code>.

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>false</code> (default),
   call <code>a.exit(reason)</code> if
   <code>!reason.equals("normal")</code>.
   */
  def exit(reason: String): Unit = self.exit(reason)
}

/**
 * This trait defines commonalities between thread-based and
 * event-based actors.
 *
 * @author Philipp Haller
 */
trait Actor {

  private[actors] val in = new Channel[Any]
  in.receiver = this

  private var rc: Channel[Any] = null
  private[actors] def reply: Channel[Any] = {
    if (rc == null) {
      rc = new Channel[Any]
      rc.receiver = this
    }
    rc
  }

  private[actors] def freshReply(): Unit = {
    rc = new Channel[Any]
    rc.receiver = this
  }

  /**
   * The behavior of an actor is specified by implementing this
   * abstract method. Note that the preferred way to create actors
   * is through the <code>actor</code> and <code>reactor</code>
   * methods defined in object <code>Actor</code>.
   */
  def act(): Unit

  /**
   * Sends <code>msg</code> to this actor (asynchronous).
   */
  def !(msg: Any): Unit = in ! msg

  /**
   * Sends <code>msg</code> to this actor and awaits reply
   * (synchronous).
   */
  def !?(msg: Any): Any = in !? msg

  private[actors] def sender: Actor
  private[actors] def pushSender(sender: Actor): unit
  private[actors] def popSender(): unit

  private[actors] var suspendActor: () => unit = _
  private[actors] var suspendActorFor: long => unit = _
  private[actors] var detachActor: PartialFunction[Any, unit] => unit = _
  private[actors] var kill: () => Unit = _

  private[actors] def scheduleActor(f: PartialFunction[Any, Unit], msg: Any)

  private[actors] def isThreaded: boolean
  private[actors] def resetActor(): unit

  resetActor()

  private val links = new HashSet[Actor]

  /**
   Links <code>self</code> to actor <code>to</code>.
   */
  def link(to: Actor): Actor = {
    links += to
    to.linkTo(this)
    to
  }

  /**
   Links <code>self</code> to actor defined by <code>body</code>.
   */
  def link(body: => Unit): Actor = {
    val actor = new ActorThread {
      def act() = body
    }
    link(actor)
    actor.start()
    actor
  }

  private[actors] def linkTo(to: Actor): Unit =
    links += to

  /**
   Unlinks <code>self</code> from actor <code>from</code>.
   */
  def unlink(from: Actor): Unit = {
    links -= from
    from.unlinkFrom(this)
  }

  private[actors] def unlinkFrom(from: Actor): Unit =
    links -= from

  var trapExit = false

  private[actors] var exitReason: String = ""

  /**
   Terminates execution of <code>self</code> with the following
   effect on linked actors:

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>true</code>, send message
   <code>Exit(self, reason)</code> to <code>a</code>.

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>false</code> (default),
   call <code>a.exit(reason)</code> if
   <code>!reason.equals("normal")</code>.
   */
  def exit(reason: String): Unit

  private[actors] def exit(from: Actor, reason: String): Unit = {
    if (from == this) {
      exit(reason)
    }
    else {
      if (trapExit)
        this ! Exit(from, reason)
      else if (!reason.equals("normal"))
        exit(reason)
    }
  }

  private[actors] def exitLinked(): Unit =
    exitLinked(exitReason, new HashSet[Actor])

  private[actors] def exitLinked(reason: String): Unit =
    exitLinked(reason, new HashSet[Actor])

  private[actors] def exitLinked(reason: String,
                                 exitMarks: HashSet[Actor]): Unit = {
    if (exitMarks contains this) {
      // we are marked, do nothing
    }
    else {
      exitMarks += this // mark this as exiting
      // exit linked processes
      val iter = links.elements
      while (iter.hasNext) {
        val linked = iter.next
        unlink(linked)
        linked.exit(this, reason)
      }
      exitMarks -= this
    }
  }
}

/**
 * Messages of this type are sent to each actor <code>a</code>
 * that is linked to an actor <code>b</code> whenever
 * <code>b</code> terminates and <code>a</code> has
 * <code>trapExit</code> set to <code>true</code>.
 *
 * @author Philipp Haller
 */
case class Exit(from: Actor, reason: String)


/**
 * This class provides an implementation for actors based on
 * threads. To be able to create instances of this class, the
 * inherited abstract method <code>act()</code> has to be
 * implemented. Note that the preferred way of creating
 * thread-based actors is through the <code>actor</code> method
 * defined in object <code>Actor</code>.
 *
 * @author Philipp Haller
 */
abstract class ActorThread extends Thread with ThreadedActor {
  override def run(): Unit = {
    try {
      act()
      if (isInterrupted())
        throw new InterruptedException
      kill()
      if (isInterrupted())
        throw new InterruptedException
      exit("normal")
    }
    catch {
      case ie: InterruptedException =>
        exitLinked()
      case t: Throwable =>
        exitLinked(t.toString())
    }
  }

  /**
   Terminates execution of <code>self</code> with the following
   effect on linked actors:

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>true</code>, send message
   <code>Exit(self, reason)</code> to <code>a</code>.

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>false</code> (default),
   call <code>a.exit(reason)</code> if
   <code>!reason.equals("normal")</code>.
   */
  def exit(reason: String): Unit = {
    exitReason = reason
    interrupt()
  }
}

/**
 * This class provides a dynamic actor proxy for normal Java
 * threads.
 *
 * @author Philipp Haller
 */
private[actors] class ActorProxy(t: Thread) extends ThreadedActor {
  def act(): Unit = {}
  /**
   Terminates execution of <code>self</code> with the following
   effect on linked actors:

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>true</code>, send message
   <code>Exit(self, reason)</code> to <code>a</code>.

   For each linked actor <code>a</code> with
   <code>trapExit</code> set to <code>false</code> (default),
   call <code>a.exit(reason)</code> if
   <code>!reason.equals("normal")</code>.
   */
  def exit(reason: String): Unit = {
    exitReason = reason
    t.interrupt()
  }
}


/**
 This object provides methods for creating, registering, and
 selecting remotely accessible actors.

 A remote actor is typically created like this:
 <pre>
 actor {
   alive(9010)
   register('myName, self)

   // behavior
 }
 </pre>

 It can be accessed by an actor running on a (possibly)
 different node by selecting it in the following way:
 <pre>
 actor {
   // ...
   <b>val</b> c = select(TcpNode("127.0.0.1", 9010), 'myName)
   c ! msg
   // ...
 }
 </pre>

 @author Philipp Haller
 */
object RemoteActor {
  import remote.NetKernel
  import remote.TcpService

  private val kernels = new scala.collection.mutable.HashMap[Actor, NetKernel]

  /**
   * Makes <code>self</code> remotely accessible on TCP port
   * <code>port</code>.
   */
  def alive(port: int): Unit = {
    val serv = new TcpService(port)
    serv.start()
    kernels += Actor.self -> serv.kernel
  }

  /**
   * Registers <code>a</code> under <code>name</code> on this
   * node.
   */
  def register(name: Symbol, a: Actor): Unit = {
    val kernel = kernels.get(Actor.self) match {
      case None => {
        val serv = new TcpService(TcpService.generatePort)
        serv.start()
        kernels += Actor.self -> serv.kernel
        serv.kernel
      }
      case Some(k) => k
    }
    kernel.register(name, a)
  }

  /**
   * Returns (a proxy for) the actor registered under
   * <code>name</code> on <code>node</code>.
   */
  def select(node: Node, name: Symbol): Actor =
    new Reactor {
      def act(): Unit = {}
      override def !(msg: Any): Unit = msg match {
        case a: AnyRef => {
          // establish remotely accessible
          // return path (sender)
          val kernel = kernels.get(Actor.self) match {
            case None => {
              val serv = new TcpService(TcpService.generatePort)
              serv.start()
              kernels += Actor.self -> serv.kernel
              serv.kernel
            }
            case Some(k) => k
          }
          kernel.send(node, name, a)
        }
        case other =>
          error("Cannot send non-AnyRef value remotely.")
      }
      override def !?(msg: Any): Any =
        error("!? not implemented for remote actors.")
    }
}


/**
 * This class represents a machine node on a TCP network.
 *
 * @author Philipp Haller
 */
case class Node(address: String, port: Int)


/**
 * <p>
 *   This class is used by our efficient message queue
 *   implementation.
 * </p>
 * <dl class="subclasses">
 *   <dt><b>Direct Known Subclasses:</b></dt>
 *   <dd>
 *     <a href="MessageQueue.html" target="contentFrame">MessageQueue</a>
 *   </dd>
 * </dl>
 */
private[actors] abstract class MessageQueueResult[Msg] {
  def msg: Msg
  def sender: Actor
}

/**
 * The class <code>MessageQueue</code> provides an efficient
 * implementation of a message queue specialized for this actor
 * library. Classes in this package are supposed to be the only
 * clients of this class.
 *
 * @author Martin Odersky, Philipp Haller
 */
private[actors] class MessageQueue[Msg] extends MessageQueueResult[Msg] {
  var msg: Msg = _
  var sender: Actor = _
  private var next: MessageQueue[Msg] = this

  def append(msg: Msg, sender: Actor) = {
    val q = new MessageQueue[Msg]
    q.msg = msg
    q.sender = sender
    q.next = next
    next = q
  }

  def extractFirst(p: Msg => boolean): MessageQueueResult[Msg] = {
    var q = this
    var qnext = q.next
    while (qnext != this) {
      if (p(qnext.msg)) {
        q.next = qnext.next
        return qnext
      }
      q = qnext
      qnext = qnext.next
    }
    null
  }

  def dequeueFirst(p: MessageQueueResult[Msg] => boolean): MessageQueueResult[Msg] = {
    var q = this
    var qnext = q.next
    while (qnext != this) {
      if (p(qnext)) {
        q.next = qnext.next
        return qnext
      }
      q = qnext
      qnext = qnext.next
    }
    null
  }
}
