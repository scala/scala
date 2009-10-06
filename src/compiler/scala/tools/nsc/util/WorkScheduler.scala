package scala.tools.nsc
package util

import scala.collection.mutable.Queue

class WorkScheduler {

  type Action = () => Unit

  private var todo = new Queue[Action]
  private var except = new Queue[Exception]

  /** Called from server: block until todo list is nonempty */
  def waitForMoreWork() = synchronized {
    while (todo.isEmpty) { wait() }
  }

  /** called from Server: test whether todo list is nonempty */
  def moreWork(): Boolean = synchronized {
    todo.nonEmpty
  }

  /** Called from server: get first action in todo list, and pop it off */
  def nextWorkItem(): Option[Action] = synchronized {
    if (!todo.isEmpty) {
      Some(todo.dequeue())
    } else None
  }

  /** Called from server: return optional exception posted by client
   *  Reset to no exception.
   */
  def pollException(): Option[Exception] = synchronized {
    if (except.isEmpty)
      None
    else {
      val result = Some(except.dequeue())
      if (!except.isEmpty)
        postWorkItem { () => }
      result
    }
  }

  /** Called from client: have action executed by server */
  def postWorkItem(action: Action) = synchronized {
    todo enqueue action
    notify()
  }

  /** Called from client: cancel all queued actions */
  def cancelQueued() = synchronized {
    todo.clear()
  }

  /** Called from client:
   *  Require an exception to be thrown on next poll.
   */
  def raise(exc: Exception) = synchronized {
    except enqueue exc
    postWorkItem { () => }
  }
}
