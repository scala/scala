/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools.util

import compat.Platform.currentTime
import scala.collection.mutable.Stack

/**
 * This abstract class implements the collection of timings. How the
 * collected timings are issued has to be implemented in subclasses.
 *
 * @author Philippe Altherr
 * @version 1.0
 */
abstract class AbstractTimer {

  //########################################################################
  // Private Fields

  /** A stack for maintaining start times */
  private val starts = new Stack[Long]()

  //########################################################################
  // Public Methods

  /** Issues a timing information (duration in milliseconds). */
  def issue(message: String, duration: Long): Unit

  /** Starts a new timer. */
  def start() {
    starts push currentTime
  }

  /** Ends the current timer. */
  def stop(message: String) {
    val stop = currentTime
    issue(message, stop - starts.pop)
  }

  /** Drops the current timer. */
  def drop() {
    starts.pop
  }

    //########################################################################
}
