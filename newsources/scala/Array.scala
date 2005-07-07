/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

final class Array[T](val length: Int) extends Cloneable with java.io.Serializable with Seq[T] {
  def apply(i: Int): T = throw new Error();
  def update(i: Int, x: T): Unit = throw new Error();
  def elements: Iterator[T] = throw new Error();
}
