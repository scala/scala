/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2006, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import scala.collection.mutable.Stack;

/**
 * This abstract class implements the collection of timings. How the
 * collected timings are issued has to be implemented in subclasses.
 */
abstract class AbstractTimer {

  //########################################################################
  // Private Fields

  /** A stack for maintaining start times */
  private val starts = new Stack[Long]();

  //########################################################################
  // Public Methods

  /** Issues a timing information (duration in milliseconds). */
  def issue(message: String, duration: Long): Unit;

  /** Starts a new timer. */
  def start() = {
    starts += System.currentTimeMillis();
  }

  /** Ends the current timer. */
  def stop(message: String): Unit = {
    val stop = System.currentTimeMillis();
    issue(message, stop - starts.pop);
  }

  /** Drops the current timer. */
  def drop(): Unit =  {
    starts.pop;
  }

    //########################################################################
}
