/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.util.control

/**
 * A marker trait indicating that the <code>Throwable</code> it is mixed
 * into is intended for flow control.
 *
 * <p>Note that <code>Throwable</code> subclasses which extend this trait
 * may extend any other <code>Throwable</code> subclass (eg.
 * <code>RuntimeException</code>) and are not required to extend
 * <code>Throwable</code> directly.</p>
 *
 * <p>Instances of <code>Throwable</code> subclasses marked in
 * this way should not normally be caught. Where catch-all behaviour is
 * required <code>ControlException</code>s should be propagated, for
 * example,</p>
 *
 * <pre>
 *  import scala.util.control.ControlException
 *
 *  try {
 *    // Body might throw arbitrarily
 * } catch {
 *   case ce : ControlException => throw ce // propagate
 *   case t : Exception => log(t)           // log and suppress
 * </pre>
 *
 * @author Miles Sabin
 */
trait ControlException extends Throwable with NoStackTrace
