package scala.collection
package parallel.mutable




import collection.mutable.HashEntry
import collection.parallel.ParIterableIterator



/** Provides functionality for hash tables with linked list buckets,
 *  enriching the data structure by fulfilling certain requirements
 *  for their parallel construction and iteration.
 */
trait ParHashTable[K, Entry >: Null <: HashEntry[K, Entry]] extends collection.mutable.HashTable[K, Entry] {

  // always initialize size map
  if (!isSizeMapDefined) sizeMapInitAndRebuild

  /** A parallel iterator returning all the entries.
   */
  abstract class EntryIterator[T, +IterRepr <: ParIterableIterator[T]]
    (private var idx: Int, private val until: Int, private val totalsize: Int, private var es: Entry)
  extends ParIterableIterator[T] {
    private val itertable = table
    private var traversed = 0
    scan()

    def entry2item(e: Entry): T
    def newIterator(idxFrom: Int, idxUntil: Int, totalSize: Int, es: Entry): IterRepr

    def hasNext = {
      es ne null
    }

    def next: T = {
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

    def split: Seq[ParIterableIterator[T]] = if (remaining > 1) {
      if (until > idx) {
        // there is at least one more slot for the next iterator
        // divide the rest of the table
        val divsz = (until - idx) / 2

        // second iterator params
        val sidx = idx + divsz + 1 // + 1 preserves iteration invariant
        val suntil = until
        val ses = itertable(sidx - 1).asInstanceOf[Entry] // sidx - 1 ensures counting from the right spot
        val stotal = calcNumElems(sidx - 1, suntil)

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
        val arrpit = new collection.parallel.BufferIterator[T](arr, 0, arr.length, signalDelegate)
        arrpit.split
      }
    } else Seq(this.asInstanceOf[IterRepr])

    private def convertToArrayBuffer(chainhead: Entry): mutable.ArrayBuffer[T] = {
      var buff = mutable.ArrayBuffer[Entry]()
      var curr = chainhead
      while (curr ne null) {
        buff += curr
        curr = curr.next
      }
      // println("converted " + remaining + " element iterator into buffer: " + buff)
      buff map { e => entry2item(e) }
    }

    private def calcNumElems(from: Int, until: Int) = {
      // find the first bucket
      val fbindex = from / sizeMapBucketSize

      // find the last bucket
      val lbindex = until / sizeMapBucketSize
      // note to self: FYI if you define lbindex as from / sizeMapBucketSize, the first branch
      // below always triggers and tests pass, so you spend a great day benchmarking and profiling

      if (fbindex == lbindex) {
        // if first and last are the same, just count between `from` and `until`
        // return this count
        countElems(from, until)
      } else {
        // otherwise count in first, then count in last
        val fbuntil = ((fbindex + 1) * sizeMapBucketSize) min itertable.length
        val fbcount = countElems(from, fbuntil)
        val lbstart = lbindex * sizeMapBucketSize
        val lbcount = countElems(lbstart, until)

        // and finally count the elements in all the buckets between first and last using a sizemap
        val inbetween = countBucketSizes(fbindex + 1, lbindex)

        // return the sum
        fbcount + inbetween + lbcount
      }
    }

    private def countElems(from: Int, until: Int) = {
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

    private def countBucketSizes(fromBucket: Int, untilBucket: Int) = {
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



object ParHashTable {
  var iters = 0
}

