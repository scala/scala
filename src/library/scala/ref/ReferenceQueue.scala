/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.ref

/**
 *  @author Sean McDirmid
 *  @author Philipp Haller
 */
class ReferenceQueue[+T <: AnyRef] {

  private[ref] val underlying: java.lang.ref.ReferenceQueue[_ <: T] = new java.lang.ref.ReferenceQueue[T]
  override def toString = underlying.toString

  protected def Wrapper(jref: java.lang.ref.Reference[_]): Option[Reference[T]] =
    jref match {
      case null => None
      case ref => Some(ref.asInstanceOf[ReferenceWithWrapper[T]].wrapper)
    }

  def poll: Option[Reference[T]] = Wrapper(underlying.poll)
  def remove: Option[Reference[T]] = Wrapper(underlying.remove)
  def remove(timeout: Long): Option[Reference[T]] = Wrapper(underlying.remove(timeout))

}
