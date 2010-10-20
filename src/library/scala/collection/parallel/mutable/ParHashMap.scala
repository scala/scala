package scala.collection.parallel
package mutable




import collection.generic._
import collection.mutable.DefaultEntry
import collection.mutable.HashEntry
import collection.mutable.HashTable



class ParHashMap[K, V] private[collection] (contents: HashTable.Contents[K, DefaultEntry[K, V]])
extends ParMap[K, V]
   with GenericParMapTemplate[K, V, ParHashMap]
   with ParMapLike[K, V, ParHashMap[K, V], collection.mutable.HashMap[K, V]]
   with ParHashTable[K, DefaultEntry[K, V]]
{
self =>
  initWithContents(contents)

  type Entry = collection.mutable.DefaultEntry[K, V]

  def this() = this(null)

  override def mapCompanion: GenericParMapCompanion[ParHashMap] = ParHashMap

  override def empty: ParHashMap[K, V] = new ParHashMap[K, V]

  def seq = new collection.mutable.HashMap[K, V](hashTableContents)

  def parallelIterator = new ParHashMapIterator(0, table.length, size, table(0).asInstanceOf[DefaultEntry[K, V]]) with SCPI

  override def size = tableSize

  def get(key: K): Option[V] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value)
  }

  override def put(key: K, value: V): Option[V] = {
    val e = findEntry(key)
    if (e == null) { addEntry(new Entry(key, value)); None }
    else { val v = e.value; e.value = value; Some(v) }
  }

  override def update(key: K, value: V): Unit = put(key, value)

  override def remove(key: K): Option[V] = {
    val e = removeEntry(key)
    if (e ne null) Some(e.value)
    else None
  }

  def += (kv: (K, V)): this.type = {
    val e = findEntry(kv._1)
    if (e == null) addEntry(new Entry(kv._1, kv._2))
    else e.value = kv._2
    this
  }

  def -=(key: K): this.type = { removeEntry(key); this }

  type SCPI = SignalContextPassingIterator[ParHashMapIterator]

  class ParHashMapIterator(start: Int, untilIdx: Int, totalSize: Int, e: DefaultEntry[K, V])
  extends EntryIterator[(K, V), ParHashMapIterator](start, untilIdx, totalSize, e) with ParIterator {
  me: SCPI =>
    def entry2item(entry: DefaultEntry[K, V]) = (entry.key, entry.value);
    def newIterator(idxFrom: Int, idxUntil: Int, totalSz: Int, es: DefaultEntry[K, V]) =
      new ParHashMapIterator(idxFrom, idxUntil, totalSz, es) with SCPI
  }

}


object ParHashMap extends ParMapFactory[ParHashMap] {
  def empty[K, V]: ParHashMap[K, V] = new ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParHashMap[K, V]] = ParHashMapCombiner.apply[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParHashMap[K, V]] = new CanCombineFromMap[K, V]
}


private[mutable] abstract class ParHashMapCombiner[K, V](private val tableLoadFactor: Int)
extends collection.parallel.BucketCombiner[(K, V), ParHashMap[K, V], DefaultEntry[K, V], ParHashMapCombiner[K, V]](ParHashMapCombiner.numblocks) {
self: EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]] =>
  private var mask = ParHashMapCombiner.discriminantmask

  def +=(elem: (K, V)) = {
    sz += 1
    val hc = elem._1.##
    val pos = hc & mask
    if (lasts(pos) eq null) {
      // initialize bucket
      heads(pos) = new Unrolled[DefaultEntry[K, V]]
      lasts(pos) = heads(pos)
    }
    // add to bucket
    lasts(pos) = lasts(pos).add(new DefaultEntry(elem._1, elem._2))
    this
  }

  def result: ParHashMap[K, V] = {
    // construct table
    val table = new AddingHashTable(size, tableLoadFactor)

    executeAndWaitResult(new FillBlocks(heads, table, 0, ParHashMapCombiner.numblocks))

    val c = table.hashTableContents
    new ParHashMap(c)
  }

  /** A hash table which will never resize itself. Knowing the number of elements in advance,
   *  it allocates the table of the required size when created.
   *
   *  Entries are added using the `insertEntry` method. This method checks whether the element
   *  exists and updates the size map.
   */
  class AddingHashTable(numelems: Int, lf: Int) extends HashTable[K, DefaultEntry[K, V]] {
    import HashTable._
    _loadFactor = lf
    table = new Array[HashEntry[K, DefaultEntry[K, V]]](capacity(sizeForThreshold(_loadFactor, numelems)))
    tableSize = 0
    threshold = newThreshold(_loadFactor, table.length)
    sizeMapInit(table.length)
    def insertEntry(e: DefaultEntry[K, V]) {
      var h = index(elemHashCode(e.key))
      var olde = table(h).asInstanceOf[DefaultEntry[K, V]]

      // check if key already exists
      var ce = olde
      while (ce ne null) {
        if (ce.key == e.key) {
          h = -1
          ce = null
        } else ce = ce.next
      }

      // if key does not already exist
      if (h != -1) {
        e.next = olde
        table(h) = e
        tableSize = tableSize + 1
        nnSizeMapAdd(h)
      }
    }
  }

  /* tasks */

  class FillBlocks(buckets: Array[Unrolled[DefaultEntry[K, V]]], table: AddingHashTable, offset: Int, howmany: Int)
  extends super.Task[Unit, FillBlocks] {
    var result = ()
    def leaf(prev: Option[Unit]) = {
      var i = offset
      val until = offset + howmany
      while (i < until) {
        fillBlock(buckets(i))
        i += 1
      }
    }
    private def fillBlock(elems: Unrolled[DefaultEntry[K, V]]) {
      var unrolled = elems
      var i = 0
      val t = table
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val elem = chunkarr(i)
          t.insertEntry(elem)
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }
    }
    def split = {
      val fp = howmany / 2
      List(new FillBlocks(buckets, table, offset, fp), new FillBlocks(buckets, table, offset + fp, howmany - fp))
    }
    def shouldSplitFurther = howmany > collection.parallel.thresholdFromSize(ParHashMapCombiner.numblocks, parallelismLevel)
  }

}


private[mutable] object ParHashMapCombiner {
  private[mutable] val discriminantbits = 5
  private[mutable] val numblocks = 1 << discriminantbits
  private[mutable] val discriminantmask = ((1 << discriminantbits) - 1) << (32 - discriminantbits)

  def apply[K, V] = new ParHashMapCombiner[K, V](HashTable.defaultLoadFactor) with EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]]
}














