/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** This class implements a simple proxy that forwards all calls to
 *  the public, non-final methods defined in class `Any` to another
 *  object self.  Those methods are:
 *  {{{
 *    def hashCode(): Int
 *    def equals(other: Any): Boolean
 *    def toString(): String
 *  }}}
 *  '''Note:''' forwarding methods in this way will most likely create
 *  an asymmetric equals method, which is not generally recommended.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 26/04/2004
 */
trait Proxy extends Any {
  def self: Any

  override def hashCode: Int = self.hashCode
  override def equals(that: Any): Boolean = that match {
    case null  => false
    case _     =>
      val x = that.asInstanceOf[AnyRef]
      (x eq this.asInstanceOf[AnyRef]) || (x eq self.asInstanceOf[AnyRef]) || (x equals self)
  }
  override def toString = "" + self
}

object Proxy {
  /** A proxy which exposes the type it is proxying for via a type parameter.
   */
  trait Typed[T] extends Any with Proxy {
    def self: T
  }
}
