/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

import runtime._

object Array {
  def copy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = src match {
    case xs: BoxedArray =>
      xs.copyTo(srcPos, dest, destPos, length)
    case _ =>
      dest match {
        case xs: BoxedArray =>
          xs.copyFrom(src, srcPos, destPos, length)
        case _ =>
          System.arraycopy(src, srcPos, dest, destPos, length)
      }
  }
}

[cloneable,serializable]
final class Array[A](_length: Int) extends Seq[A] {
  def length: Int = throw new Error();
  def apply(i: Int): A = throw new Error();
  def update(i: Int, x: A): Unit = throw new Error();
  def elements: Iterator[A] = throw new Error();
}
