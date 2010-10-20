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

    def hasNext = es != null

    def next = {
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

    def split: Seq[ParIterableIterator[T]] = if (remaining > 1) {
      if ((until - idx) > 1) {
        // there is at least one more slot for the next iterator
        // divide the rest of the table
        val divsz = (until - idx) / 2

        // second iterator params
        val sidx = idx + divsz
        val suntil = until
        val ses = itertable(sidx).asInstanceOf[Entry]
        val stotal = calcNumElems(sidx, suntil)

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
      while (curr != null) {
        buff += curr
        curr = curr.next
      }
      buff map { e => entry2item(e) }
    }

    private def calcNumElems(from: Int, until: Int) = {
      // find the first bucket
      val fbindex = from / sizeMapBucketSize

      // find the last bucket
      val lbindex = from / sizeMapBucketSize

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





