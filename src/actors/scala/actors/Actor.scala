package scala.actors

import scala.collection.mutable.HashSet

object Actor {

  private[actors] val selfs = new java.util.WeakHashMap(16, 0.5f)

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

  def actor(body: => Unit): ActorThread = synchronized {
    val actor = new ActorThread {
      def act() = body
    }
    actor.start()
    actor
  }

  def actor[a](ch: Channel[a])(body: => Unit): ActorThread = synchronized {
    val actor = new ActorThread {
      def act() = body
    }
    ch.receiver = actor
    actor.start()
    actor
  }

  def reactor(body: => Unit): Reactor = synchronized {
    val reactor = new Reactor {
      def act() = body
    }
    reactor.start()
    reactor
  }

  def receive[a](f: PartialFunction[Any, a]): a =
    self.in.receive(f)

  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R =
    self.in.receiveWithin(msec)(f)

  def react(f: PartialFunction[Any, Unit]): Nothing =
    self.in.react(f)

  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing =
    self.in.reactWithin(msec)(f)

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

  def from(r: Actor): FromReceive =
    new FromReceive(r)

  private[actors] class FromReceive(r: Actor) {
    def receive[a](f: PartialFunction[Any, a]): a =
      self.in.receiveFrom(r)(f)
  }

  def sender: Actor = self.sender

  def reply(msg: Any): Unit = sender.reply ! msg

  def reply(): Unit = reply(())

  def forward(msg: Any): Unit = self.in.forward(msg)

  private[actors] trait Body[T] {
    def orElse(other: => T): T
    def andThen(other: => T): T
  }

  implicit def mkBody(body: => Unit) = new Body[Unit] {
    def orElse(other: => Unit): Unit = choose(body, other)
    def andThen(other: => Unit): Unit = seq(body, other)
  }

  def choose(alt1: => Unit, alt2: => Unit): Unit = {
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

  def loop(b: => Unit): Unit = {
    val s = self
    s.kill = () => { b; s.kill() }
    b
  }

  def seq(b1: => Unit, b2: => Unit): Unit = {
    val s = self
    s.kill = () => { b2 }
    b1
  }

  def link(to: Actor): Actor = self.link(to)
  def link(body: => Unit): Actor = self.link(body)
  def unlink(from: Actor): Unit = self.unlink(from)
  def exit(reason: String): Unit = self.exit(reason)
}

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

  /*
   Specification of behavior
   */
  def act(): Unit

  def !(msg: Any): Unit = in ! msg
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

  def link(to: Actor): Actor = {
    links += to
    to.linkTo(this)
    to
  }

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

  def unlink(from: Actor): Unit = {
    links -= from
    from.unlinkFrom(this)
  }

  private[actors] def unlinkFrom(from: Actor): Unit =
    links -= from

  var trapExit = false

  private[actors] var exitReason: String = ""

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

case class Exit(from: Actor, reason: String)


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

  def exit(reason: String): Unit = {
    exitReason = reason
    interrupt()
  }
}

class ActorProxy(t: Thread) extends ThreadedActor {
  def act(): Unit = {}
  def exit(reason: String): Unit = {
    exitReason = reason
    t.interrupt()
  }
}


object RemoteActor {
  import remote.NetKernel
  import remote.TcpService

  private val kernels = new scala.collection.mutable.HashMap[Actor, NetKernel]

  def alive(port: int): Unit = {
    val serv = new TcpService(port)
    serv.start()
    kernels += Actor.self -> serv.kernel
  }

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

abstract class Node
case class TcpNode(address: String, port: Int) extends Node
case class JxtaNode(group: String) extends Node


private[actors] abstract class MessageQueueResult[Msg] {
  def msg: Msg
  def sender: Actor
}

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
