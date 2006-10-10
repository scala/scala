/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 This trait is part of the thread-based implementation of
 actors.

 @author Philipp Haller
 */
trait ThreadedActor extends Actor {
  private val lastSenders = new scala.collection.mutable.Stack[Actor]
  private[actors] def sender: Actor = {
    if (lastSenders.isEmpty) null
    else lastSenders.top
  }
  private[actors] def pushSender(sender: Actor) = { lastSenders.push(sender) }
  private[actors] def popSender(): Unit = { lastSenders.pop }

  private[actors] def isThreaded = true

  private[actors] def scheduleActor(f: PartialFunction[Any, Unit], msg: Any) = {
    notify()
  }

  private[actors] def tick(): Unit = {}

  private[actors] def resetActor() = {
    suspendActor = () => wait()
    suspendActorFor = (msec: long) => wait(msec)
    resumeActor = () => notify()
    detachActor = (f: PartialFunction[Any, Unit]) => wait()
    kill = () => {}
  }

  resetActor()
}
