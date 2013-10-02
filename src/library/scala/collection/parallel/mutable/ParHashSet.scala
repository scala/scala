/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel.mutable



import scala.collection.generic._
import scala.collection.mutable.FlatHashTable
import scala.collection.parallel.Combiner
import scala.collection.mutable.UnrolledBuffer
import scala.collection.parallel.Task



/** A parallel hash set.
 *
 *  `ParHashSet` is a parallel set which internally keeps elements within a hash table.
 *  It uses linear probing to resolve collisions.
 *
 *  @tparam T        type of the elements in the $coll.
 *
 *  @define Coll `ParHashSet`
 *  @define coll parallel hash set
 *
 *  @author Aleksandar Prokopec
 *  @see  [[http://docs.scala-lang.org/overviews/parallel-collections/concrete-parallel-collections.html#parallel_hash_tables Scala's Parallel Collections Library overview]]
 *  section on Parallel Hash Tables for more information.
 */
@SerialVersionUID(1L)
class ParHashSet[T] private[collection] (contents: FlatHashTable.Contents[T])
extends ParSet[T]
   with GenericParTemplate[T, ParHashSet]
   with ParSetLike[T, ParHashSet[T], scala.collection.mutable.HashSet[T]]
   with ParFlatHashTable[T]
   with Serializable
{
  initWithContents(contents)
  // println("----> new par hash set!")
  // java.lang.Thread.dumpStack
  // println(debugInformation)

  def this() = this(null)

  override def companion = ParHashSet

  override def empty = new ParHashSet

  override def iterator = splitter

  override def size = tableSize

  def clear() = clearTable()

  override def seq = new scala.collection.mutable.HashSet(hashTableContents)

  def +=(elem: T) = {
    addElem(elem)
    this
  }

  def -=(elem: T) = {
    removeElem(elem)
    this
  }

  override def stringPrefix = "ParHashSet"

  def contains(elem: T) = containsElem(elem)

  def splitter = new ParHashSetIterator(0, table.length, size)

  class ParHashSetIterator(start: Int, iteratesUntil: Int, totalElements: Int)
  extends ParFlatHashTableIterator(start, iteratesUntil, totalElements) {
    def newIterator(start: Int, until: Int, total: Int) = new ParHashSetIterator(start, until, total)
  }

  private def writeObject(s: java.io.ObjectOutputStream) {
    serializeTo(s)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    init(in, x => ())
  }

  import scala.collection.DebugUtils._
  override def debugInformation = buildString {
    append =>
    append("Parallel flat hash table set")
    append("No. elems: " + tableSize)
    append("Table length: " + table.length)
    append("Table: ")
    append(arrayString(table, 0, table.length))
    append("Sizemap: ")
    append(arrayString(sizemap, 0, sizemap.length))
  }

}


/** $factoryInfo
 *  @define Coll `mutable.ParHashSet`
 *  @define coll parallel hash set
 */
object ParHashSet extends ParSetFactory[ParHashSet] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParHashSet[T]] = new GenericCanCombineFrom[T]

  override def newBuilder[T]: Combiner[T, ParHashSet[T]] = newCombiner

  override def newCombiner[T]: Combiner[T, ParHashSet[T]] = ParHashSetCombiner.apply[T]
}


private[mutable] abstract class ParHashSetCombiner[T](private val tableLoadFactor: Int)
extends scala.collection.parallel.BucketCombiner[T, ParHashSet[T], AnyRef, ParHashSetCombiner[T]](ParHashSetCombiner.numblocks)
with scala.collection.mutable.FlatHashTable.HashUtils[T] {
//self: EnvironmentPassingCombiner[T, ParHashSet[T]] =>
  private val nonmasklen = ParHashSetCombiner.nonmasklength
  private val seedvalue = 27

  def +=(elem: T) = {
    val entry = elemToEntry(elem)
    sz += 1
    val hc = improve(entry.hashCode, seedvalue)
    val pos = hc >>> nonmasklen
    if (buckets(pos) eq null) {
      // initialize bucket
      buckets(pos) = new UnrolledBuffer[AnyRef]
    }
    // add to bucket
    buckets(pos) += entry
    this
  }

  def result: ParHashSet[T] = {
    val contents = if (size >= ParHashSetCombiner.numblocks * sizeMapBucketSize) parPopulate else seqPopulate
    new ParHashSet(contents)
  }

  private def parPopulate: FlatHashTable.Contents[T] = {
    // construct it in parallel
    val table = new AddingFlatHashTable(size, tableLoadFactor, seedvalue)
    val (inserted, leftovers) = combinerTaskSupport.executeAndWaitResult(new FillBlocks(buckets, table, 0, buckets.length))
    var leftinserts = 0
    for (entry <- leftovers) leftinserts += table.insertEntry(0, table.tableLength, entry)
    table.setSize(leftinserts + inserted)
    table.hashTableContents
  }

  private def seqPopulate: FlatHashTable.Contents[T] = {
    // construct it sequentially
    // TODO parallelize by keeping separate size maps and merging them
    val tbl = new FlatHashTable[T] {
      sizeMapInit(table.length)
      seedvalue = ParHashSetCombiner.this.seedvalue
      for {
        buffer <- buckets
        if buffer ne null
        entry <- buffer
      } addEntry(entry)
    }
    tbl.hashTableContents
  }

  /* classes */

  /** A flat hash table which doesn't resize itself. It accepts the number of elements
   *  it has to take and allocates the underlying hash table in advance.
   *  Elements can only be added to it. The final size has to be adjusted manually.
   *  It is internal to `ParHashSet` combiners.
   */
  class AddingFlatHashTable(numelems: Int, lf: Int, inseedvalue: Int) extends FlatHashTable[T] {
    _loadFactor = lf
    table = new Array[AnyRef](capacity(FlatHashTable.sizeForThreshold(numelems, _loadFactor)))
    tableSize = 0
    threshold = FlatHashTable.newThreshold(_loadFactor, table.length)
    seedvalue = inseedvalue
    sizeMapInit(table.length)

    override def toString = "AFHT(%s)".format(table.length)

    def tableLength = table.length

    def setSize(sz: Int) = tableSize = sz

    /**
     *  The elements are added using the `insertElem` method. This method accepts three
     *  arguments:
     *
     *  @param insertAt      where to add the element (set to -1 to use its hashcode)
     *  @param comesBefore   the position before which the element should be added to
     *  @param newEntry      the element to be added
     *
     *  If the element is to be inserted at the position corresponding to its hash code,
     *  the table will try to add the element in such a position if possible. Collisions are resolved
     *  using linear hashing, so the element may actually have to be added to a position
     *  that follows the specified one. In the case that the first unoccupied position
     *  comes after `comesBefore`, the element is not added and the method simply returns -1,
     *  indicating that it couldn't add the element in a position that comes before the
     *  specified one.
     *  If the element is already present in the hash table, it is not added, and this method
     *  returns 0. If the element is added, it returns 1.
     */
    def insertEntry(insertAt: Int, comesBefore: Int, newEntry : AnyRef): Int = {
      var h = insertAt
      if (h == -1) h = index(newEntry.hashCode)
      var curEntry = table(h)
      while (null != curEntry) {
        if (curEntry == newEntry) return 0
        h = h + 1 // we *do not* do `(h + 1) % table.length` here, because we'll never overflow!!
        if (h >= comesBefore) return -1
        curEntry = table(h)
      }
      table(h) = newEntry

      // this is incorrect since we set size afterwards anyway and a counter
      // like this would not even work:
      //
      //   tableSize = tableSize + 1
      //
      // furthermore, it completely bogs down the parallel
      // execution when there are multiple workers

      nnSizeMapAdd(h)
      1
    }
  }

  /* tasks */

  class FillBlocks(buckets: Array[UnrolledBuffer[AnyRef]], table: AddingFlatHashTable, val offset: Int, val howmany: Int)
  extends Task[(Int, UnrolledBuffer[AnyRef]), FillBlocks] {
    var result = (Int.MinValue, new UnrolledBuffer[AnyRef])

    def leaf(prev: Option[(Int, UnrolledBuffer[AnyRef])]) {
      var i = offset
      var totalinserts = 0
      var leftover = new UnrolledBuffer[AnyRef]()
      while (i < (offset + howmany)) {
        val (inserted, intonextblock) = fillBlock(i, buckets(i), leftover)
        totalinserts += inserted
        leftover = intonextblock
        i += 1
      }
      result = (totalinserts, leftover)
    }
    private val blocksize = table.tableLength >> ParHashSetCombiner.discriminantbits
    private def blockStart(block: Int) = block * blocksize
    private def nextBlockStart(block: Int) = (block + 1) * blocksize
    private def fillBlock(block: Int, elems: UnrolledBuffer[AnyRef], leftovers: UnrolledBuffer[AnyRef]): (Int, UnrolledBuffer[AnyRef]) = {
      val beforePos = nextBlockStart(block)

      // store the elems
      val (elemsIn, elemsLeft) = if (elems != null) insertAll(-1, beforePos, elems) else (0, UnrolledBuffer[AnyRef]())

      // store the leftovers
      val (leftoversIn, leftoversLeft) = insertAll(blockStart(block), beforePos, leftovers)

      // return the no. of stored elements tupled with leftovers
      (elemsIn + leftoversIn, elemsLeft concat leftoversLeft)
    }
    private def insertAll(atPos: Int, beforePos: Int, elems: UnrolledBuffer[AnyRef]): (Int, UnrolledBuffer[AnyRef]) = {
      val leftovers = new UnrolledBuffer[AnyRef]
      var inserted = 0

      var unrolled = elems.headPtr
      var i = 0
      val t = table
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val entry = chunkarr(i)
          val res = t.insertEntry(atPos, beforePos, entry)
          if (res >= 0) inserted += res
          else leftovers += entry
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }

      // slower:
      // var it = elems.iterator
      // while (it.hasNext) {
      //   val elem = it.next
      //   val res = table.insertEntry(atPos, beforePos, elem.asInstanceOf[T])
      //   if (res >= 0) inserted += res
      //   else leftovers += elem
      // }

      (inserted, leftovers)
    }
    def split = {
      val fp = howmany / 2
      List(new FillBlocks(buckets, table, offset, fp), new FillBlocks(buckets, table, offset + fp, howmany - fp))
    }
    override def merge(that: FillBlocks) {
      // take the leftovers from the left task, store them into the block of the right task
      val atPos = blockStart(that.offset)
      val beforePos = blockStart(that.offset + that.howmany)
      val (inserted, remainingLeftovers) = insertAll(atPos, beforePos, this.result._2)

      // anything left after trying the store the left leftovers is added to the right task leftovers
      // and a new leftovers set is produced in this way
      // the total number of successfully inserted elements is adjusted accordingly
      result = (this.result._1 + that.result._1 + inserted, remainingLeftovers concat that.result._2)
    }
    def shouldSplitFurther = howmany > scala.collection.parallel.thresholdFromSize(ParHashMapCombiner.numblocks, combinerTaskSupport.parallelismLevel)
  }

}


private[parallel] object ParHashSetCombiner {
  private[mutable] val discriminantbits = 5
  private[mutable] val numblocks = 1 << discriminantbits
  private[mutable] val discriminantmask = ((1 << discriminantbits) - 1)
  private[mutable] val nonmasklength = 32 - discriminantbits

  def apply[T] = new ParHashSetCombiner[T](FlatHashTable.defaultLoadFactor) {} //with EnvironmentPassingCombiner[T, ParHashSet[T]]
}

