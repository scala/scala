/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/**
 * An object that provides methods for capturing unique references, and
 * for accessing unique fields.
 *
 * @author Philipp Haller
 * @since 2.9
 */
package object unique {

  trait Captured[A] {
    def captured: A
    def capturedBy[B](other: B): A = captured
  }

  implicit def mkCaptured[A](x: A) = new Captured[A] {
    def captured: A = x
  }

  def swap[A, B <: A](to: A, from: B): A = to

}
