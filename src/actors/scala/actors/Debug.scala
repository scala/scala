/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

/**
 * Provides methods for generating debugging output.
 *
 * @author Philipp Haller
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
object Debug extends Logger("") {}

private[actors] class Logger(tag: String) {
  private var lev = 2

  def level = lev
  def level_= (lev: Int) = { this.lev = lev }

  private val tagString = if (tag == "") "" else " ["+tag+"]"

  def info(s: String) =
    if (lev > 2) System.out.println("Info" + tagString + ": " + s)

  def warning(s: String) =
    if (lev > 1) System.err.println("Warning" + tagString + ": " + s)

  def error(s: String) =
    if (lev > 0) System.err.println("Error" + tagString + ": " + s)

  def doInfo(b: => Unit) =
    if (lev > 2) b

  def doWarning(b: => Unit) =
    if (lev > 1) b

  def doError(b: => Unit) =
    if (lev > 0) b
}
