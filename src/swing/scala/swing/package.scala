package scala

/**
 * Useful imports that don't have wrappers.
 */
package object swing {
  type Point = java.awt.Point
  type Dimension = java.awt.Dimension
  type Rectangle = java.awt.Rectangle
  type Insets = java.awt.Insets

  type Graphics2D = java.awt.Graphics2D
  type Color = java.awt.Color
  type Image = java.awt.Image
  type Font = java.awt.Font

  implicit lazy val reflectiveCalls     = language.reflectiveCalls
  implicit lazy val implicitConversions = language.implicitConversions

  private[swing] def ifNull[A](o: Object, a: A): A = if(o eq null) a else o.asInstanceOf[A]
  private[swing] def toOption[A](o: Object): Option[A] = if(o eq null) None else Some(o.asInstanceOf[A])
  private[swing] def toAnyRef(x: Any): AnyRef = x.asInstanceOf[AnyRef]
}
