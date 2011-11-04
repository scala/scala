/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.util.concurrent.locks.ReentrantLock
import scala.tools.nsc.util.Exceptional
import Exceptional.unwrap
import Line._

/** Encapsulation of a single line in the repl.  The concurrency
 *  infrastructure arose to deal with signals so SIGINT could be
 *  trapped without losing the repl session, but it will be useful
 *  in ways beyond that.  Each line obtains a thread and the repl
 *  waits on a condition indicating that either the line has
 *  completed or failed.
 */
class Line[+T](val code: String, classLoader: ClassLoader, body: => T) {
  private var _state: State              = Running
  private var _result: Option[Any]       = None
  private var _caught: Option[Throwable] = None
  private val lock                       = new ReentrantLock()
  private val finished                   = lock.newCondition()

  private def withLock[T](body: => T) = {
    lock.lock()
    try body
    finally lock.unlock()
  }
  private def setState(state: State) = withLock {
    _state = state
    finished.signal()
  }
  // private because it should be called by the manager.
  private def cancel() = if (running) setState(Cancelled)

  private def runAndSetState[T](body: => T) {
    try     {           _result = Some(body) ; setState(Done)  }
    catch   { case t => _caught = Some(t)    ; setState(Threw) }
  }

  // This is where the line thread is created and started.
  private val _thread: Thread =
    io.newThread(_ setContextClassLoader classLoader)(runAndSetState(body))

  def state     = _state
  def thread    = _thread
  def alive     = thread.isAlive
  def runaway   = !success && alive
  def success   = _state == Done
  def running   = _state == Running

  def caught() = { await() ; _caught.orNull }
  def get()    = {
    await()
    _result getOrElse sys.error("Called get with no result.  Code: " + code)
  }
  def await()  = withLock { while (running) finished.await() }
}

object Line {
  // seconds to let a runaway thread live before calling stop()
  private val HUNTER_KILLER_DELAY = 5

  // A line opens in state Running, and will eventually
  // transition to Threw (an exception was caught), Cancelled
  // (the line was explicitly cancelled, presumably by SIGINT)
  // or Done (success).
  sealed abstract class State
  case object Running extends State
  case object Threw extends State
  case object Cancelled extends State
  case object Done extends State

  class Manager(classLoader: ClassLoader) {
    /** Override to add behavior for runaway lines.  This method will
     *  be called if a line thread is still running five seconds after
     *  it has been cancelled.
     */
    def onRunaway(line: Line[_]): Unit = ()

    private var _current: Option[Line[_]] = None
    def current = _current

    def clear() = {
      _current foreach (_.cancel())
      _current = None
    }
    def set[T](code: String)(body: => T) = {
      val line = new Line(code, classLoader, body)
      _current = Some(line)
      line
    }
    def running = _current.isDefined
    def cancel() = {
      current foreach { line =>
        line.thread.interrupt()
        line.cancel()
        if (line.runaway)
          io.timer(HUNTER_KILLER_DELAY) { if (line.alive) onRunaway(line) }
      }
    }
  }
}
