package scala.actors

import Actor._

case object TIMEOUT

class SuspendActorException extends Throwable {
  override def fillInStackTrace(): Throwable = {
    this
  }
}

class Channel[Msg] {

  private[actors] var receiver: Actor = synchronized {
    // basically Actor.self, but can be null
    val t = Thread.currentThread()
    if (t.isInstanceOf[ActorThread])
      t.asInstanceOf[ActorThread]
    else {
      val a = selfs.get(t).asInstanceOf[Actor]
      a
    }
  }

  private var received: Msg = _

  private val waitingForNone = (m: Msg) => false
  private var waitingFor: Msg => boolean = waitingForNone
  private var waitingForSender: Actor = null

  private val messageQueue = new MessageQueue[Msg]

  private def send(msg: Msg, sender: Actor) = receiver.synchronized {
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

  def !(msg: Msg): unit = send(msg, self)

  def !?(msg: Msg): Any = {
    self.freshReply()
    this ! msg
    self.reply.receiveFrom(receiver) {
      case x => x
    }
  }

  def forward(msg: Msg): unit = send(msg, receiver.sender)

  def receive[R](f: PartialFunction[Msg, R]): R = {
    assert(self == receiver, "receive from channel belonging to other actor")
    assert(receiver.isThreaded, "receive invoked from reactor")
    receiver.synchronized {
      waitingFor = f.isDefinedAt
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
      }
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
    assert(self == receiver, "receive from channel belonging to other actor")
    assert(receiver.isThreaded, "receive invoked from reactor")
    receiver.synchronized {
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

  def receiveWithin[R](msec: long)(f: PartialFunction[Any, R]): R = {
    assert(self == receiver, "receive from channel belonging to other actor")
    assert(receiver.isThreaded, "receive invoked from reactor")
    receiver.synchronized {
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

  def react(f: PartialFunction[Any, Unit]): Nothing = {
    assert(self == receiver, "react on channel belonging to other actor")
    receiver.synchronized {
      waitingFor = f.isDefinedAt
      val q = messageQueue.extractFirst(waitingFor)
      if (q != null) {
        received = q.msg
        receiver.pushSender(q.sender)
        waitingFor = waitingForNone
        receiver.scheduleActor(f, received)

        // would like:
        // receiver.continuation = f
        // receiver.message = received
        // receiver.resume()
      }
      else synchronized {
        receiver.detachActor(f)
      }
      throw new SuspendActorException
    }
  }

  def reactWithin(msec: long)(f: PartialFunction[Any, Unit]): Nothing = {
    assert(self == receiver, "react on channel belonging to other actor")
    receiver.synchronized {
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
