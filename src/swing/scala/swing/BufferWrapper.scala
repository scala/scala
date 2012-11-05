/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import scala.collection.mutable.Buffer
import scala.collection.Iterator

/**
 * Default partial implementation for buffer adapters.
 */
protected[swing] abstract class BufferWrapper[A] extends Buffer[A] { outer =>
  def clear() { for (i <- 0 until length) remove(0) }
  def update(n: Int, a: A) {
    remove(n)
    insertAt(n, a)
  }
  def insertAll(n: Int, elems: Traversable[A]) {
    var i = n
    for (el <- elems) {
      insertAt(i, el)
      i += 1
    }
  }
  protected def insertAt(n: Int, a: A)

  def +=:(a: A): this.type = { insertAt(0, a); this }
  def iterator = Iterator.range(0,length).map(apply(_))
}
