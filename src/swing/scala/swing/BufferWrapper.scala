/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.swing

import scala.collection.mutable.Buffer
import scala.collection.IndexedSeq

/**
 * Default partial implementation for buffer adapters.
 */
protected[swing] abstract class BufferWrapper[A] extends Buffer[A] { outer =>
  def clear { for (i <- 0 to length) remove(0) }
  def update(n: Int, a: A) {
    remove(0)
    insertAt(n, a)
  }
  def insertAll(n: Int, elems: scala.collection.Traversable[A]) {
    var i = n
    for (el <- elems) {
      insertAt(i, el)
      i += 1
    }
  }
  protected def insertAt(n: Int, a: A)

  override def readOnly: IndexedSeq[A] = new IndexedSeq[A] {
    def length = outer.length
    def apply(idx : Int) = outer.apply(idx)
    override def stringPrefix = outer.stringPrefix + "RO"
  }
  def +=:(a: A): this.type = { insertAt(0, a); this }
  def iterator = Iterator.range(0,length).map(apply(_))
}
