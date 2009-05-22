package scala.tools.nsc.util

import scala.collection.mutable.Queue

class WorkScheduler {

  type Action = () => Unit

  private var todo = new Queue[Action]

  /** Called from server */
  def waitForMoreWork() = synchronized {
    do { wait() } while (todo.isEmpty)
  }

  /** called from Server */
  def moreWork(): Boolean = synchronized {
    todo.nonEmpty
  }

  /** Called from server */
  def nextWorkItem(): Option[Action] = synchronized {
    if (!todo.isEmpty) Some(todo.dequeue()) else None
  }

  /** Called from client */
  def postWorkItem(action: Action) {
    todo enqueue action
    notify()
  }

  /** Called from client */
  def cancel() = synchronized {
    todo.clear()
  }

  /** Called from client */
  def raise(exc: Exception) = synchronized {
    todo.clear()
    todo enqueue (() => throw exc)
  }
}

