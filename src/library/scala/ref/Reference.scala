/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.ref

/**
 * @see <code>java.lang.ref.Reference</code>
 * @author Sean McDirmid
 */
trait Reference[+T <: AnyRef] extends Function0[T] {
  /** return the underlying value */
  def apply(): T
  /** return <code>Some</code> underlying if it hasn't been collected, otherwise <code>None</code> */
  def get: Option[T]
  override def toString = get.map(_.toString).getOrElse("<deleted>")
  def clear(): Unit
  def enqueue(): Boolean
  def isEnqueued(): Boolean
}
