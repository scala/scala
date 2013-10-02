/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package parallel.mutable

import scala.collection.mutable.HashEntry
import scala.collection.parallel.IterableSplitter

/** Provides functionality for hash tables with linked list buckets,
 *  enriching the data structure by fulfilling certain requirements
 *  for their parallel construction and iteration.
 */
trait ParHashTable[K, Entry >: Null <: HashEntry[K, Entry]] extends scala.collection.mutable.HashTable[K, Entry] {

  override def alwaysInitSizeMap = true

  /** A parallel iterator returning all the entries.
   */
  abstract class EntryIterator[T, +IterRepr <: IterableSplitter[T]]
  (private var idx: Int, private val until: Int, private val totalsize: Int, private var es: Entry)
  extends IterableSplitter[T] with SizeMapUtils {
    private val itertable = table
    private var traversed = 0
    scan()

    def entry2item(e: Entry): T
    def newIterator(idxFrom: Int, idxUntil: Int, totalSize: Int, es: Entry): IterRepr

    def hasNext = {
      es ne null
    }

    def next(): T = {
      val res = es
      es = es.next
      scan()
      traversed += 1
      entry2item(res)
    }

    def scan() {
      while (es == null && idx < until) {
        es = itertable(idx).asInstanceOf[Entry]
        idx = idx + 1
      }
    }

    def remaining = totalsize - traversed

    private[parallel] override def debugInformation = {
      buildString {
        append =>
        append("/--------------------\\")
        append("Parallel hash table entry iterator")
        append("total hash table elements: " + tableSize)
        append("pos: " + idx)
        append("until: " + until)
        append("traversed: " + traversed)
        append("totalsize: " + totalsize)
        append("current entry: " + es)
        append("underlying from " + idx + " until " + until)
        append(itertable.slice(idx, until).map(x => if (x != null) x.toString else "n/a").mkString(" | "))
        append("\\--------------------/")
      }
    }

    def dup = newIterator(idx, until, totalsize, es)

    def split: Seq[IterableSplitter[T]] = if (remaining > 1) {
      if (until > idx) {
        // there is at least one more slot for the next iterator
        // divide the rest of the table
        val divsz = (until - idx) / 2

        // second iterator params
        val sidx = idx + divsz + 1 // + 1 preserves iteration invariant
        val suntil = until
        val ses = itertable(sidx - 1).asInstanceOf[Entry] // sidx - 1 ensures counting from the right spot
        val stotal = calcNumElems(sidx - 1, suntil, table.length, sizeMapBucketSize)

        // first iterator params
        val fidx = idx
        val funtil = idx + divsz
        val fes = es
        val ftotal = totalsize - stotal

        Seq(
          newIterator(fidx, funtil, ftotal, fes),
          newIterator(sidx, suntil, stotal, ses)
        )
      } else {
        // otherwise, this is the last entry in the table - all what remains is the chain
        // so split the rest of the chain
        val arr = convertToArrayBuffer(es)
        val arrpit = new scala.collection.parallel.BufferSplitter[T](arr, 0, arr.length, signalDelegate)
        arrpit.split
      }
    } else Seq(this.asInstanceOf[IterRepr])

    private def convertToArrayBuffer(chainhead: Entry): mutable.ArrayBuffer[T] = {
      val buff = mutable.ArrayBuffer[Entry]()
      var curr = chainhead
      while (curr ne null) {
        buff += curr
        curr = curr.next
      }
      // println("converted " + remaining + " element iterator into buffer: " + buff)
      buff map { e => entry2item(e) }
    }

    protected def countElems(from: Int, until: Int) = {
      var c = 0
      var idx = from
      var es: Entry = null
      while (idx < until) {
        es = itertable(idx).asInstanceOf[Entry]
        while (es ne null) {
          c += 1
          es = es.next
        }
        idx += 1
      }
      c
    }

    protected def countBucketSizes(fromBucket: Int, untilBucket: Int) = {
      var c = 0
      var idx = fromBucket
      while (idx < untilBucket) {
        c += sizemap(idx)
        idx += 1
      }
      c
    }
  }
}
