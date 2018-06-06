package scala

import java.lang.reflect.{ AccessibleObject => jAccessibleObject }

package object reflect {

  def classTag[T](implicit ctag: ClassTag[T]) = ctag

  /** Make a java reflection object accessible, if it is not already
   *  and it is possible to do so. If a SecurityException is thrown in the
   *  attempt, it is caught and discarded.
   */
  def ensureAccessible[T <: jAccessibleObject](m: T): T = {
    if (!m.isAccessible) {
      try m setAccessible true
      catch { case _: SecurityException => } // does nothing
    }
    m
  }

  // anchor for the class tag materialization macro emitted during tag materialization in Implicits.scala
  // implementation is hardwired into `scala.reflect.reify.Taggers`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  // todo. once we have implicit macros for tag generation, we can remove this anchor
  private[scala] def materializeClassTag[T](): ClassTag[T] = macro ???
}

/** An exception that indicates an error during Scala reflection */
case class ScalaReflectionException(msg: String) extends Exception(msg)
