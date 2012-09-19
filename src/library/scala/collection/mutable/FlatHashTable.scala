/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
package mutable


/** An implementation class backing a `HashSet`.
 *
 *  This trait is used internally. It can be mixed in with various collections relying on
 *  hash table as an implementation.
 *
 *  @define coll flat hash table
 *  @define cannotStoreNull '''Note''': A $coll cannot store `null` elements.
 *  @since 2.3
 *  @tparam A   the type of the elements contained in the $coll.
 */
trait FlatHashTable[A] extends FlatHashTable.HashUtils[A] {
  import FlatHashTable._

  private final def tableDebug = false

  @transient private[collection] var _loadFactor = defaultLoadFactor

  /** The actual hash table.
   */
  @transient protected var table: Array[AnyRef] = new Array(initialCapacity)

  /** The number of mappings contained in this hash table.
   */
  @transient protected var tableSize = 0

  /** The next size value at which to resize (capacity * load factor).
   */
  @transient protected var threshold: Int = newThreshold(_loadFactor, initialCapacity)

  /** The array keeping track of number of elements in 32 element blocks.
   */
  @transient protected var sizemap: Array[Int] = null

  @transient protected var seedvalue: Int = tableSizeSeed

  import HashTable.powerOfTwo

  protected def capacity(expectedSize: Int) = if (expectedSize == 0) 1 else powerOfTwo(expectedSize)

  /** The initial size of the hash table.
   */
  def initialSize: Int = 32

  private def initialCapacity = capacity(initialSize)

  protected def randomSeed = seedGenerator.get.nextInt()

  protected def tableSizeSeed = Integer.bitCount(table.length - 1)

  /**
   * Initializes the collection from the input stream. `f` will be called for each element
   * read from the input stream in the order determined by the stream. This is useful for
   * structures where iteration order is important (e.g. LinkedHashSet).
   *
   * The serialization format expected is the one produced by `serializeTo`.
   */
  private[collection] def init(in: java.io.ObjectInputStream, f: A => Unit) {
    in.defaultReadObject

    _loadFactor = in.readInt()
    assert(_loadFactor > 0)

    val size = in.readInt()
    tableSize = 0
    assert(size >= 0)

    table = new Array(capacity(sizeForThreshold(size, _loadFactor)))
    threshold = newThreshold(_loadFactor, table.size)

    seedvalue = in.readInt()

    val smDefined = in.readBoolean()
    if (smDefined) sizeMapInit(table.length) else sizemap = null

    var index = 0
    while (index < size) {
      val elem = in.readObject().asInstanceOf[A]
      f(elem)
      addEntry(elem)
      index += 1
    }
  }

  /**
   * Serializes the collection to the output stream by saving the load factor, collection
   * size and collection elements. `foreach` determines the order in which the elements are saved
   * to the stream. To deserialize, `init` should be used.
   */
  private[collection] def serializeTo(out: java.io.ObjectOutputStream) {
    out.defaultWriteObject
    out.writeInt(_loadFactor)
    out.writeInt(tableSize)
    out.writeInt(seedvalue)
    out.writeBoolean(isSizeMapDefined)
    iterator.foreach(out.writeObject)
  }

  /** Finds an entry in the hash table if such an element exists. */
  protected def findEntry(elem: A): Option[A] = {
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry && entry != elem) {
      h = (h + 1) % table.length
      entry = table(h)
    }
    if (null == entry) None else Some(entry.asInstanceOf[A])
  }

  /** Checks whether an element is contained in the hash table. */
  protected def containsEntry(elem: A): Boolean = {
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry && entry != elem) {
      h = (h + 1) % table.length
      entry = table(h)
    }
    null != entry
  }

  /** Add entry if not yet in table.
   *  @return Returns `true` if a new entry was added, `false` otherwise.
   */
  protected def addEntry(elem: A) : Boolean = {
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry) {
      if (entry == elem) return false
      h = (h + 1) % table.length
      entry = table(h)
      //Statistics.collisions += 1
    }
    table(h) = elem.asInstanceOf[AnyRef]
    tableSize = tableSize + 1
    nnSizeMapAdd(h)
    if (tableSize >= threshold) growTable()
    true
  }

  /** Removes an entry from the hash table, returning an option value with the element, or `None` if it didn't exist. */
  protected def removeEntry(elem: A) : Option[A] = {
    if (tableDebug) checkConsistent()
    def precedes(i: Int, j: Int) = {
      val d = table.length >> 1
      if (i <= j) j - i < d
      else i - j > d
    }
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry) {
      if (entry == elem) {
        var h0 = h
        var h1 = (h0 + 1) % table.length
        while (null != table(h1)) {
          val h2 = index(elemHashCode(table(h1).asInstanceOf[A]))
          //Console.println("shift at "+h1+":"+table(h1)+" with h2 = "+h2+"? "+(h2 != h1)+precedes(h2, h0)+table.length)
          if (h2 != h1 && precedes(h2, h0)) {
            //Console.println("shift "+h1+" to "+h0+"!")
            table(h0) = table(h1)
            h0 = h1
          }
          h1 = (h1 + 1) % table.length
        }
        table(h0) = null
        tableSize -= 1
        nnSizeMapRemove(h0)
        if (tableDebug) checkConsistent()
        return Some(entry.asInstanceOf[A])
      }
      h = (h + 1) % table.length
      entry = table(h)
    }
    None
  }

  protected def iterator: Iterator[A] = new AbstractIterator[A] {
    private var i = 0
    def hasNext: Boolean = {
      while (i < table.length && (null == table(i))) i += 1
      i < table.length
    }
    def next(): A =
      if (hasNext) { i += 1; table(i - 1).asInstanceOf[A] }
      else Iterator.empty.next
  }

  private def growTable() {
    val oldtable = table
    table = new Array[AnyRef](table.length * 2)
    tableSize = 0
    nnSizeMapReset(table.length)
    seedvalue = tableSizeSeed
    threshold = newThreshold(_loadFactor, table.length)
    var i = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (null != entry) addEntry(entry.asInstanceOf[A])
      i += 1
    }
    if (tableDebug) checkConsistent()
  }

  private def checkConsistent() {
    for (i <- 0 until table.length)
      if (table(i) != null && !containsEntry(table(i).asInstanceOf[A]))
        assert(false, i+" "+table(i)+" "+table.mkString)
  }

  /* Size map handling code */

  /*
   * The following three methods (nn*) modify a size map only if it has been
   * initialized, that is, if it's not set to null.
   *
   * The size map logically divides the hash table into `sizeMapBucketSize` element buckets
   * by keeping an integer entry for each such bucket. Each integer entry simply denotes
   * the number of elements in the corresponding bucket.
   * Best understood through an example, see:
   * table   = [/, 1, /, 6, 90, /, -3, 5]    (8 entries)
   * sizemap = [     2     |     3      ]    (2 entries)
   * where sizeMapBucketSize == 4.
   *
   */
  protected def nnSizeMapAdd(h: Int) = if (sizemap ne null) {
    val p = h >> sizeMapBucketBitSize
    sizemap(p) += 1
  }

  protected def nnSizeMapRemove(h: Int) = if (sizemap ne null) {
    sizemap(h >> sizeMapBucketBitSize) -= 1
  }

  protected def nnSizeMapReset(tableLength: Int) = if (sizemap ne null) {
    val nsize = calcSizeMapSize(tableLength)
    if (sizemap.length != nsize) sizemap = new Array[Int](nsize)
    else java.util.Arrays.fill(sizemap, 0)
  }

  private[collection] final def totalSizeMapBuckets = (table.length - 1) / sizeMapBucketSize + 1

  protected def calcSizeMapSize(tableLength: Int) = (tableLength >> sizeMapBucketBitSize) + 1

  // discards the previous sizemap and only allocates a new one
  protected def sizeMapInit(tableLength: Int) {
    sizemap = new Array[Int](calcSizeMapSize(tableLength))
  }

  // discards the previous sizemap and populates the new one
  protected def sizeMapInitAndRebuild() {
    // first allocate
    sizeMapInit(table.length)

    // rebuild
    val totalbuckets = totalSizeMapBuckets
    var bucketidx = 0
    var tableidx = 0
    var tbl = table
    var tableuntil = sizeMapBucketSize min tbl.length
    while (bucketidx < totalbuckets) {
      var currbucketsz = 0
      while (tableidx < tableuntil) {
        if (tbl(tableidx) ne null) currbucketsz += 1
        tableidx += 1
      }
      sizemap(bucketidx) = currbucketsz
      tableuntil += sizeMapBucketSize
      bucketidx += 1
    }
  }

  private[collection] def printSizeMap() {
    println(sizemap.mkString("szmap: [", ", ", "]"))
  }

  private[collection] def printContents() {
    println(table.mkString("[", ", ", "]"))
  }

  protected def sizeMapDisable() = sizemap = null

  protected def isSizeMapDefined = sizemap ne null

  protected def alwaysInitSizeMap = false

  /* End of size map handling code */

  protected final def index(hcode: Int) = {
    // version 1 (no longer used - did not work with parallel hash tables)
    // improve(hcode) & (table.length - 1)

    // version 2 (allows for parallel hash table construction)
    val improved = improve(hcode, seedvalue)
    val ones = table.length - 1
    (improved >>> (32 - java.lang.Integer.bitCount(ones))) & ones

    // version 3 (solves SI-5293 in most cases, but such a case would still arise for parallel hash tables)
    // val hc = improve(hcode)
    // val bbp = blockbitpos
    // val ones = table.length - 1
    // val needed = Integer.bitCount(ones)
    // val blockbits = ((hc >>> bbp) & 0x1f) << (needed - 5)
    // val rest = ((hc >>> (bbp + 5)) << bbp) | (((1 << bbp) - 1) & hc)
    // val restmask = (1 << (needed - 5)) - 1
    // val improved = blockbits | (rest & restmask)
    // improved
  }

  protected def clearTable() {
    var i = table.length - 1
    while (i >= 0) { table(i) = null; i -= 1 }
    tableSize = 0
    nnSizeMapReset(table.length)
  }

  private[collection] def hashTableContents = new FlatHashTable.Contents[A](
    _loadFactor,
    table,
    tableSize,
    threshold,
    seedvalue,
    sizemap
  )

  protected def initWithContents(c: FlatHashTable.Contents[A]) = {
    if (c != null) {
      _loadFactor = c.loadFactor
      table = c.table
      tableSize = c.tableSize
      threshold = c.threshold
      seedvalue = c.seedvalue
      sizemap = c.sizemap
    }
    if (alwaysInitSizeMap && sizemap == null) sizeMapInitAndRebuild
  }

}


private[collection] object FlatHashTable {

  /** Creates a specific seed to improve hashcode of a hash table instance
   *  and ensure that iteration order vulnerabilities are not 'felt' in other
   *  hash tables.
   *
   *  See SI-5293.
   */
  final def seedGenerator = new ThreadLocal[scala.util.Random] {
    override def initialValue = new scala.util.Random
  }

  /** The load factor for the hash table; must be < 500 (0.5)
   */
  def defaultLoadFactor: Int = 450
  final def loadFactorDenum = 1000

  def sizeForThreshold(size: Int, _loadFactor: Int) = scala.math.max(32, (size.toLong * loadFactorDenum / _loadFactor).toInt)

  def newThreshold(_loadFactor: Int, size: Int) = {
    val lf = _loadFactor
    assert(lf < (loadFactorDenum / 2), "loadFactor too large; must be < 0.5")
    (size.toLong * lf / loadFactorDenum ).toInt
  }

  class Contents[A](
    val loadFactor: Int,
    val table: Array[AnyRef],
    val tableSize: Int,
    val threshold: Int,
    val seedvalue: Int,
    val sizemap: Array[Int]
  )

  trait HashUtils[A] {
    protected final def sizeMapBucketBitSize = 5
    // so that:
    protected final def sizeMapBucketSize = 1 << sizeMapBucketBitSize

    protected def elemHashCode(elem: A) =
      if (elem == null) throw new IllegalArgumentException("Flat hash tables cannot contain null elements.")
      else elem.hashCode()

    protected final def improve(hcode: Int, seed: Int) = {
      //var h: Int = hcode + ~(hcode << 9)
      //h = h ^ (h >>> 14)
      //h = h + (h << 4)
      //h ^ (h >>> 10)

      val improved= scala.util.hashing.byteswap32(hcode)

      // for the remainder, see SI-5293
      // to ensure that different bits are used for different hash tables, we have to rotate based on the seed
      val rotation = seed % 32
      val rotated = (improved >>> rotation) | (improved << (32 - rotation))
      rotated
    }
  }

}


