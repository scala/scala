package scala.tools.nsc.util

import scala.collection.mutable.Queue

class WorkScheduler {

  type Action = () => Unit

  private var todo = new Queue[Action]
  private var except: Option[Exception] = None
  private var working = false

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
      working = true
      Some(todo.dequeue())
    } else None
  }

  /** Called from server: raise any exception posted by client */
  def pollException() = synchronized {
    except match {
      case Some(exc) => throw exc
      case None =>
    }
  }

  /** Called from server: mark workitem as finished (influences
   *  meaning of raise)
   */
  def doneWorkItem() = synchronized {
    working = false
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
   *  If work in progress, raise an exception in it next
   *  time pollException is called.
   */
  def raise(exc: Exception) = synchronized {
    if (working) except = Some(exc)
  }
}
