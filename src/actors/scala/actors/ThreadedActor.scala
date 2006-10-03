package scala.actors

/**
 This trait is part of the thread-based implementation of
 actors.

 @author Philipp Haller
 */
trait ThreadedActor extends Actor {
  private val lastSenders = new scala.collection.mutable.Stack[Actor]
  def sender: Actor = {
    if (lastSenders.isEmpty) null
    else lastSenders.top
  }
  def pushSender(sender: Actor) = { lastSenders.push(sender) }
  def popSender(): Unit = { lastSenders.pop }

  def isThreaded = true

  def scheduleActor(f: PartialFunction[Any, Unit], msg: Any) = {
    notify()
  }

  def resetActor() = {
    suspendActor = () => wait()
    suspendActorFor = (msec: long) => wait(msec)
    detachActor = (f: PartialFunction[Any, Unit]) => wait()
    kill = () => {}
  }

  resetActor()
}
