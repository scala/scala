/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent

/** The class <code>SyncChannel</code> ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 10/03/2003
 */
class SyncChannel[a] {
  private var data: a = _
  private var reading = false
  private var writing = false

  def await(cond: => Boolean) = while (!cond) wait()

  def write(x: a) = synchronized {
    await(!writing)
    data = x
    writing = true
    if (reading) notifyAll()
    else await(reading)
  }

  def read: a = synchronized {
    await(!reading)
    reading = true
    await(writing)
    val x = data
    writing = false
    reading = false
    notifyAll()
    x
  }
}
