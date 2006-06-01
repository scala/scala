/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RichInt.scala 7625 2006-05-30 10:47:58 +0000 (Tue, 30 May 2006) odersky $
package scala.runtime

final class RichInt(x: Int) {
  def until(y: Int): Iterator[Int] = Iterator.range(x, y)
  def to(y: Int): Iterator[Int] = Iterator.range(x, y + 1)
}



