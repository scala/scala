package scala.actors

trait Reactor extends Actor {
  private var lastSender: Actor = null
  def sender: Actor = lastSender
  def pushSender(sender: Actor): Unit = lastSender = sender
  def popSender(): Unit = lastSender = null

  def isThreaded = false

  private[actors] var continuation: PartialFunction[Any, Unit] = null
  private[actors] var timeoutPending = false

  def scheduleActor(f: PartialFunction[Any, Unit], msg: Any) = {
    if (f == null && continuation == null) {
      // do nothing (timeout is handled instead)
    }
    else {
      val task = new ActorTask(this,
                               if (f == null) continuation else f,
                               msg)
      Scheduler.execute(task)
    }
  }

  def defaultDetachActor: PartialFunction[Any, Unit] => Unit =
    (f: PartialFunction[Any, Unit]) => {
      continuation = f
      throw new SuspendActorException
    }

  def resetActor(): Unit = {
    detachActor = defaultDetachActor
    suspendActor = () => error("suspendActor called on reactor.")
    suspendActorFor = (msec: long) => error("suspendActorFor called on reactor.")
    kill = () => {}
  }

  resetActor()

  def start(): Unit = {
    Scheduler.execute(new StartTask(this))
  }

  def exit(reason: String): Unit = {
    exitReason = reason
    Thread.currentThread().interrupt()
  }
}

abstract class Reaction extends Runnable {
  def actor: Reactor
}

class StartTask(a: Reactor) extends Reaction {
  def actor = a

  def run(): Unit = {
    val t = Thread.currentThread()
    val saved = Actor.selfs.get(t).asInstanceOf[Actor]
    Actor.selfs.put(t, a)
    try {
      a.act()
      if (Thread.currentThread().isInterrupted())
        throw new InterruptedException
      a.kill()
      if (Thread.currentThread().isInterrupted())
        throw new InterruptedException
      a.exit("normal")
    }
    catch {
      case _: InterruptedException => a.exitLinked()
      case d: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Throwable => {
        a.exit(t.toString())
      }
    }
    finally {
      Actor.selfs.put(t, saved)
    }
  }
}

class ActorTask(a: Reactor,
                f: PartialFunction[Any, Unit],
                msg: Any) extends Reaction {
  def actor = a

  def run(): Unit = {
    val t = Thread.currentThread()
    val saved = Actor.selfs.get(t).asInstanceOf[Actor]
    Actor.selfs.put(t, a)
    try {
      f(msg)
      if (Thread.currentThread().isInterrupted())
        throw new InterruptedException
      a.kill()
      if (Thread.currentThread().isInterrupted())
        throw new InterruptedException
      a.exit("normal")
    }
    catch {
      case _: InterruptedException => a.exitLinked()
      case d: SuspendActorException => {
        // do nothing (continuation is already saved)
      }
      case t: Throwable => {
        a.exit(t.toString())
      }
    }
    finally {
      Actor.selfs.put(t, saved)
    }
  }
}
