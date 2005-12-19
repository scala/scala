/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

final class Array[A](_length: Int) extends Cloneable with java.io.Serializable with Seq[A] {
  def length: Int = throw new Error();
  def apply(i: Int): A = throw new Error();
  def update(i: Int, x: A): Unit = throw new Error();
  def elements: Iterator[A] = throw new Error();
}
