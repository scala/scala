package scala.collection.parallel.mutable




import collection.generic._
import collection.mutable.HashSet
import collection.mutable.FlatHashTable
import collection.parallel.Combiner
import collection.parallel.EnvironmentPassingCombiner
import collection.parallel.UnrolledBuffer




class ParHashSet[T] private[collection] (contents: FlatHashTable.Contents[T])
extends ParSet[T]
   with GenericParTemplate[T, ParHashSet]
   with ParSetLike[T, ParHashSet[T], collection.mutable.HashSet[T]]
   with ParFlatHashTable[T]
{
  initWithContents(contents)
  // println("----> new par hash set!")
  // java.lang.Thread.dumpStack
  // println(debugInformation)

  def this() = this(null)

  override def companion = ParHashSet

  override def empty = new ParHashSet

  override def iterator = parallelIterator

  override def size = tableSize

  def seq = new HashSet(hashTableContents)

  def +=(elem: T) = {
    addEntry(elem)
    this
  }

  def -=(elem: T) = {
    removeEntry(elem)
    this
  }

  def contains(elem: T) = containsEntry(elem)

  def parallelIterator = new ParHashSetIterator(0, table.length, size) with SCPI

  type SCPI = SignalContextPassingIterator[ParHashSetIterator]

  class ParHashSetIterator(start: Int, iteratesUntil: Int, totalElements: Int)
  extends ParFlatHashTableIterator(start, iteratesUntil, totalElements) with ParIterator {
  me: SCPI =>
    def newIterator(start: Int, until: Int, total: Int) = new ParHashSetIterator(start, until, total) with SCPI
  }

  import collection.DebugUtils._
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
 *  @define Coll mutable.ParSet
 *  @define coll mutable parallel set
 */
object ParHashSet extends ParSetFactory[ParHashSet] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParHashSet[T]] = new GenericCanCombineFrom[T]

  override def newBuilder[T]: Combiner[T, ParHashSet[T]] = newCombiner

  override def newCombiner[T]: Combiner[T, ParHashSet[T]] = ParHashSetCombiner.apply[T]
}


private[mutable] abstract class ParHashSetCombiner[T](private val tableLoadFactor: Int)
extends collection.parallel.BucketCombiner[T, ParHashSet[T], Any, ParHashSetCombiner[T]](ParHashSetCombiner.numblocks)
with collection.mutable.FlatHashTable.HashUtils[T] {
self: EnvironmentPassingCombiner[T, ParHashSet[T]] =>
  private var mask = ParHashSetCombiner.discriminantmask
  private var nonmasklen = ParHashSetCombiner.nonmasklength

  def +=(elem: T) = {
    sz += 1
    val hc = improve(elemHashCode(elem))
    val pos = hc >>> nonmasklen
    if (buckets(pos) eq null) {
      // initialize bucket
      buckets(pos) = new UnrolledBuffer[Any]
    }
    // add to bucket
    buckets(pos) += elem
    this
  }

  def result: ParHashSet[T] = {
    val contents = if (size >= ParHashSetCombiner.numblocks * sizeMapBucketSize) parPopulate else seqPopulate
    new ParHashSet(contents)
  }

  private def parPopulate: FlatHashTable.Contents[T] = {
    // construct it in parallel
    val table = new AddingFlatHashTable(size, tableLoadFactor)
    val (inserted, leftovers) = executeAndWaitResult(new FillBlocks(buckets, table, 0, buckets.length))
    var leftinserts = 0
    for (elem <- leftovers) leftinserts += table.insertEntry(0, table.tableLength, elem.asInstanceOf[T])
    table.setSize(leftinserts + inserted)
    table.hashTableContents
  }

  private def seqPopulate: FlatHashTable.Contents[T] = {
    // construct it sequentially
    // TODO parallelize by keeping separate size maps and merging them
    val tbl = new FlatHashTable[T] {
      sizeMapInit(table.length)
    }
    for {
      buffer <- buckets;
      if buffer ne null;
      elem <- buffer
    } tbl.addEntry(elem.asInstanceOf[T])
    tbl.hashTableContents
  }

  /* classes */

  /** A flat hash table which doesn't resize itself. It accepts the number of elements
   *  it has to take and allocates the underlying hash table in advance.
   *  Elements can only be added to it. The final size has to be adjusted manually.
   *  It is internal to `ParHashSet` combiners.
   *
   */
  class AddingFlatHashTable(numelems: Int, lf: Int) extends FlatHashTable[T] {
    _loadFactor = lf
    table = new Array[AnyRef](capacity(FlatHashTable.sizeForThreshold(numelems, _loadFactor)))
    tableSize = 0
    threshold = FlatHashTable.newThreshold(_loadFactor, table.length)
    sizeMapInit(table.length)

    def tableLength = table.length

    def setSize(sz: Int) = tableSize = sz

    /**
     *  The elements are added using the `insertEntry` method. This method accepts three
     *  arguments:
     *
     *  @param insertAt      where to add the element (set to -1 to use its hashcode)
     *  @param comesBefore   the position before which the element should be added to
     *  @param elem          the element to be added
     *
     *  If the element is to be inserted at the position corresponding to its hash code,
     *  the table will try to add the element in such a position if possible. Collisions are resolved
     *  using linear hashing, so the element may actually have to be added to a position
     *  that follows the specified one. In the case that the first unoccupied position
     *  comes after `comesBefore`, the element is not added and the method simply returns `-1`,
     *  indicating that it couldn't add the element in a position that comes before the
     *  specified one.
     *  If the element is already present in the hash table, it is not added, and this method
     *  returns 0. If the element is added, it returns 1.
     */
    def insertEntry(insertAt: Int, comesBefore: Int, elem: T): Int = {
      var h = insertAt
      // if (h == -1) h = index(elemHashCode(elem))
      // var entry = table(h)
      // while (null != entry) {
      //   if (entry == elem) return 0
      //   h = (h + 1) // we *do not* do `(h + 1) % table.length` here, because we don't overlap!!
      //   if (h >= comesBefore) return -1
      //   entry = table(h)
      // }
      // table(h) = elem.asInstanceOf[AnyRef]
      // tableSize = tableSize + 1
      // nnSizeMapAdd(h)
      1
    }
  }

  /* tasks */

  class FillBlocks(buckets: Array[UnrolledBuffer[Any]], table: AddingFlatHashTable, val offset: Int, val howmany: Int)
  extends super.Task[(Int, UnrolledBuffer[Any]), FillBlocks] {
    var result = (Int.MinValue, new UnrolledBuffer[Any]);
    def leaf(prev: Option[(Int, UnrolledBuffer[Any])]) {
      var i = offset
      var totalinserts = 0
      var leftover = new UnrolledBuffer[Any]()
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
    private def fillBlock(block: Int, elems: UnrolledBuffer[Any], leftovers: UnrolledBuffer[Any]): (Int, UnrolledBuffer[Any]) = {
      val beforePos = nextBlockStart(block)

      // store the elems
      val (elemsIn, elemsLeft) = if (elems != null) insertAll(-1, beforePos, elems) else (0, UnrolledBuffer[Any]())

      // store the leftovers
      val (leftoversIn, leftoversLeft) = insertAll(blockStart(block), beforePos, leftovers)

      // return the no. of stored elements tupled with leftovers
      (elemsIn + leftoversIn, elemsLeft concat leftoversLeft)
    }
    private def insertAll(atPos: Int, beforePos: Int, elems: UnrolledBuffer[Any]): (Int, UnrolledBuffer[Any]) = {
      var leftovers = new UnrolledBuffer[Any]
      var inserted = 0

      // var unrolled = elems.headPtr
      // var i = 0
      // var t = table
      // while (unrolled ne null) {
      //   val chunkarr = unrolled.array
      //   val chunksz = unrolled.size
      //   while (i < chunksz) {
      //     val elem = chunkarr(i)
      //     val res = t.insertEntry(atPos, beforePos, elem.asInstanceOf[T])
      //     if (res >= 0) inserted += 1
      //     else leftovers += elem
      //     i += 1
      //   }
      //   i = 0
      //   unrolled = unrolled.next
      // }

      // slower:
      var it = elems.iterator
      while (it.hasNext) {
        val elem = it.next
        val res = table.insertEntry(atPos, beforePos, elem.asInstanceOf[T])
        if (res >= 0) inserted += res
        else leftovers += elem
      }

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
    def shouldSplitFurther = howmany > collection.parallel.thresholdFromSize(ParHashMapCombiner.numblocks, parallelismLevel)
  }

}


private[mutable] object ParHashSetCombiner {
  private[mutable] val discriminantbits = 5
  private[mutable] val numblocks = 1 << discriminantbits
  private[mutable] val discriminantmask = ((1 << discriminantbits) - 1);
  private[mutable] val nonmasklength = 32 - discriminantbits

  def apply[T] = new ParHashSetCombiner[T](FlatHashTable.defaultLoadFactor) with EnvironmentPassingCombiner[T, ParHashSet[T]]
}














