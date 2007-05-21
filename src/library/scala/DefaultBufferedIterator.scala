/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
import Predef._
import collection.mutable.{Buffer, ListBuffer}

abstract class DefaultBufferedIterator[+A] extends BufferedIterator[A] {
  private[this] var lookahead : List[A] = Nil

  protected[this] def putBack(a : Any) : Unit = lookahead = a.asInstanceOf[A] :: lookahead

  override protected def defaultPeek : A = throw new NoSuchElementException
  override def hasNext = !lookahead.isEmpty || super.hasNext

  /** used to fill lookahead buffer */
  protected def fill : Seq[A]

  override def peekList(sz : Int) : Seq[A] = {
    if (sz == 0) return lookahead
    else if (sz == 1) {
      if (!lookahead.isEmpty) return lookahead
      val next = fill
      lookahead = next.toList
      return lookahead
    }
    var sz0 = sz
    var hd = lookahead
    while (sz0 > 0) {
      if (hd.isEmpty) { // add n elements
        import scala.collection.mutable.ArrayBuffer
        val buf0 = new ArrayBuffer[A]
        while (sz0 > 0 && {
          val next = fill
          if (next.isEmpty) false
          else {
            buf0 ++= next
            sz0 = sz0 - next.length
            true
          }
        }) {}
        lookahead = lookahead ::: buf0.toList
        return lookahead
      }
      sz0 = sz0 - 1
      hd = hd.tail
    }
    lookahead
  }
  override def next : A = {
    val lst = peekList(1)
    if (lst.isEmpty) throw new NoSuchElementException
    lookahead = lookahead.tail
    lst(0)
  }
}
