/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package parallel.mutable

import collection.parallel.IterableSplitter

/** Parallel flat hash table.
 *
 *  @tparam T      type of the elements in the $coll.
 *  @define coll   table
 *  @define Coll   `ParFlatHashTable`
 *
 *  @author Aleksandar Prokopec
 */
trait ParFlatHashTable[T] extends collection.mutable.FlatHashTable[T] {

  override def alwaysInitSizeMap = true

  abstract class ParFlatHashTableIterator(var idx: Int, val until: Int, val totalsize: Int)
  extends IterableSplitter[T] with SizeMapUtils {
    import collection.DebugUtils._

    private var traversed = 0
    private val itertable = table

    if (hasNext) scan()

    private def scan() {
      while (itertable(idx) eq null) {
        idx += 1
      }
    }

    private def checkbounds() = if (idx >= itertable.length) {
      throw new IndexOutOfBoundsException(idx.toString)
    }

    def newIterator(index: Int, until: Int, totalsize: Int): IterableSplitter[T]

    def remaining = totalsize - traversed
    def hasNext = traversed < totalsize
    def next() = if (hasNext) {
      val r = itertable(idx).asInstanceOf[T]
      traversed += 1
      idx += 1
      if (hasNext) scan()
      r
    } else Iterator.empty.next
    def dup = newIterator(idx, until, totalsize)
    def split = if (remaining > 1) {
      val divpt = (until + idx) / 2

      val fstidx = idx
      val fstuntil = divpt
      val fsttotal = calcNumElems(idx, divpt, itertable.length, sizeMapBucketSize)
      val fstit = newIterator(fstidx, fstuntil, fsttotal)

      val sndidx = divpt
      val snduntil = until
      val sndtotal = remaining - fsttotal
      val sndit = newIterator(sndidx, snduntil, sndtotal)

      Seq(fstit, sndit)
    } else Seq(this)

    override def debugInformation = buildString {
      append =>
      append("Parallel flat hash table iterator")
      append("---------------------------------")
      append("Traversed/total: " + traversed + " / " + totalsize)
      append("Table idx/until: " + idx + " / " + until)
      append("Table length: " + itertable.length)
      append("Table: ")
      append(arrayString(itertable, 0, itertable.length))
      append("Sizemap: ")
      append(arrayString(sizemap, 0, sizemap.length))
    }

    protected def countElems(from: Int, until: Int) = {
      var count = 0
      var i = from
      while (i < until) {
        if (itertable(i) ne null) count += 1
        i += 1
      }
      count
    }

    protected def countBucketSizes(frombucket: Int, untilbucket: Int) = {
      var count = 0
      var i = frombucket
      while (i < untilbucket) {
        count += sizemap(i)
        i += 1
      }
      count
    }

    private def check() = if (table.slice(idx, until).count(_ != null) != remaining) {
      println("Invariant broken: " + debugInformation)
      assert(false)
    }
  }

}
