/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

final class Array[A](val length: Int) extends Cloneable with java.io.Serializable with Seq[A] {
  def apply(i: Int): A = throw new Error();
  def update(i: Int, x: A): Unit = throw new Error();
  def elements: Iterator[A] = throw new Error();
}
