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

  def concat[T](xs: Array[T]*) = {
    var len = 0
    for (val x <- xs) {
      len = len + x.length
    }
    val result = new Array[T](len)
    var start = 0
    for (val x <- xs) {
      copy(x, 0, result, start, x.length)
      start = start + x.length
    }
    result
  }
}

[cloneable,serializable]
final class Array[a](_length: Int) extends Seq[a] {
  def length: Int = throw new Error();
  def apply(i: Int): a = throw new Error();
  def update(i: Int, x: a): Unit = throw new Error();
  def elements: Iterator[a] = throw new Error();
  def subArray(from: Int, end: Int): Array[a] = throw new Error();
  def filter(p: a => Boolean): Array[a] = throw new Error();
  def map[b](f: a => b): Array[b] = throw new Error();
  def flatMap[b](f: a => Array[b]): Array[b] = throw new Error();
}
