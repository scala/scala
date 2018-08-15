package scala
package collection.immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import collection.{Iterator, MapFactory, StrictOptimizedIterableOps, StrictOptimizedMapOps}
import collection.Hashing.{computeHash, keepBits}
import scala.annotation.unchecked.{uncheckedVariance => uV}
import java.lang.{Integer, String, System}

import scala.collection.mutable.{Builder, ImmutableBuilder}

/** This class implements immutable maps using a hash trie.
  *
  *  '''Note:''' The builder of this hash map may return specialized representations for small maps.
  *
  *  @tparam K      the type of the keys contained in this hash map.
  *  @tparam V      the type of the values associated with the keys.
  *  @author  Martin Odersky
  *  @author  Tiark Rompf
  *  @since   2.3
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#hash-tries "Scala's Collection Library overview"]]
  *  section on `Hash Tries` for more information.
  *  @define Coll `immutable.OldHashMap`
  *  @define coll immutable hash map
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@deprecated("Use the new CHAMP-based HashMap; OldHashMap is provided for compatibility only", "2.13.0")
sealed abstract class OldHashMap[K, +V]
  extends AbstractMap[K, V]
    with MapOps[K, V, OldHashMap, OldHashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, OldHashMap[K, V]]
    with StrictOptimizedMapOps[K, V, OldHashMap, OldHashMap[K, V]] {

  import OldHashMap.{bufferSize, liftMerger, Merger, MergeFunction, nullToEmpty}

  override def mapFactory: MapFactory[OldHashMap] = OldHashMap

  def remove(key: K): OldHashMap[K, V] = removed0(key, computeHash(key), 0)

  final def updated[V1 >: V](key: K, value: V1): OldHashMap[K, V1] =
    updated0(key, computeHash(key), 0, value, null, null)

  @`inline` override final def +[V1 >: V](kv: (K, V1)): OldHashMap[K, V1] = updated(kv._1, kv._2)

  def get(key: K): Option[V] = get0(key, computeHash(key), 0)

  def split: Seq[OldHashMap[K, V]] = Seq(this)

  /** Creates a new map which is the merge of this and the argument hash map.
    *
    *  Uses the specified collision resolution function if two keys are the same.
    *  The collision resolution function will always take the first argument from
    *  `this` hash map and the second from `that`.
    *
    *  The `merged` method is on average more performant than doing a traversal and reconstructing a
    *  new immutable hash map from scratch, or `++`.
    *
    *  @tparam V1      the value type of the other hash map
    *  @param that     the other hash map
    *  @param mergef   the merge function or null if the first key-value pair is to be picked
    */
  def merged[V1 >: V](that: OldHashMap[K, V1])(mergef: MergeFunction[K, V1]): OldHashMap[K, V1] = merge0(that, 0, liftMerger(mergef))

  protected def updated0[V1 >: V](key: K, hash: Int, level: Int, value: V1, kv: (K, V1), merger: Merger[K, V1]): OldHashMap[K, V1]

  protected def removed0(key: K, hash: Int, level: Int): OldHashMap[K, V]

  protected def get0(key: K, hash: Int, level: Int): Option[V]

  protected def merge0[V1 >: V](that: OldHashMap[K, V1], level: Int, merger: Merger[K, V1]): OldHashMap[K, V1]

  protected def filter0(p: ((K, V)) => Boolean, negate: Boolean, level: Int, buffer: Array[OldHashMap[K, V @uV]], offset0: Int): OldHashMap[K, V]

  protected def contains0(key: K, hash: Int, level: Int): Boolean

  override final def contains(key: K): Boolean = contains0(key, computeHash(key), 0)

  override def tail: OldHashMap[K, V] = this - head._1

  override def init: OldHashMap[K, V] = this - last._1

  override def filter(pred: ((K, V)) => Boolean): OldHashMap[K, V] = {
    val buffer = new Array[OldHashMap[K, V]](bufferSize(size))
    nullToEmpty(filter0(pred, negate = false, 0, buffer, 0))
  }

  override def filterNot(pred: ((K, V)) => Boolean): OldHashMap[K, V] = {
    val buffer = new Array[OldHashMap[K, V]](bufferSize(size))
    nullToEmpty(filter0(pred, negate = true, 0, buffer, 0))
  }

  override protected[this] def className: String = "OldHashMap"

}

/**
  * $factoryInfo
  * @define Coll `immutable.OldHashMap`
  * @define coll immutable hash map
  */
@SerialVersionUID(3L)
object OldHashMap extends MapFactory[OldHashMap] {

  def empty[K, V]: OldHashMap[K, V] = EmptyOldHashMap.asInstanceOf[OldHashMap[K, V]]

  def from[K, V](it: collection.IterableOnce[(K, V)]): OldHashMap[K, V] =
    it match {
      case hm: OldHashMap[K, V] => hm
      case _ => (newBuilder[K, V] ++= it).result()
    }

  def newBuilder[K, V]: Builder[(K, V), OldHashMap[K, V]] =
    new ImmutableBuilder[(K, V), OldHashMap[K, V]](empty) {
      def addOne(elem: (K, V)): this.type = { elems = elems + elem; this }
    }

  private[collection] abstract class Merger[A, B] {
    def apply(kv1: (A, B), kv2: (A, B)): (A, B)
    def invert: Merger[A, B]
  }

  private type MergeFunction[A1, B1] = ((A1, B1), (A1, B1)) => (A1, B1)

  private def liftMerger[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] =
    if (mergef == null) defaultMerger.asInstanceOf[Merger[A1, B1]] else liftMerger0(mergef)

  private[this] val defaultMerger : Merger[Any, Any] = liftMerger0((a,b) => a)

  private[this] def liftMerger0[A1, B1](mergef: MergeFunction[A1, B1]): Merger[A1, B1] = new Merger[A1, B1] {
    self =>
    def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv1, kv2)
    val invert: Merger[A1, B1] = new Merger[A1, B1] {
      def apply(kv1: (A1, B1), kv2: (A1, B1)): (A1, B1) = mergef(kv2, kv1)
      def invert: Merger[A1, B1] = self
    }
  }

  // utility method to create a HashTrieMap from two leaf OldHashMaps (OldHashMap1 or OldHashMapCollision1) with non-colliding hash code)
  private def makeHashTrieMap[A, B](hash0: Int, elem0: OldHashMap[A, B], hash1: Int, elem1:OldHashMap[A, B], level: Int, size: Int) : HashTrieMap[A, B] = {
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[OldHashMap[A,B]](2)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieMap[A, B](bitmap, elems, size)
    } else {
      val elems = new Array[OldHashMap[A,B]](1)
      val bitmap = (1 << index0)
      elems(0) = makeHashTrieMap(hash0, elem0, hash1, elem1, level + 5, size)
      new HashTrieMap[A, B](bitmap, elems, size)
    }
  }

  /**
    * Calculates the maximum buffer size given the maximum possible total size of the trie-based collection
    * @param size the maximum size of the collection to be generated
    * @return the maximum buffer size
    */
  @`inline` private def bufferSize(size: Int): Int = math.min(size + 6, 32 * 7)

  /**
    * In many internal operations the empty map is represented as null for performance reasons. This method converts
    * null to the empty map for use in public methods
    */
  @`inline` private def nullToEmpty[A, B](m: OldHashMap[A, B]): OldHashMap[A, B] = if (m eq null) empty[A, B] else m

  private object EmptyOldHashMap extends OldHashMap[Any, Nothing] {

    override def isEmpty: Boolean = true
    override def knownSize: Int = 0
    protected def updated0[V1 >: Nothing](key: Any, hash: Int, level: Int, value: V1, kv: (Any, V1), merger: Merger[Any, V1]): OldHashMap[Any, V1] =
      new OldHashMap.OldHashMap1(key, hash, value, kv)

    protected def removed0(key: Any, hash: Int, level: Int): OldHashMap[Any, Nothing] = this

    protected def get0(key: Any, hash: Int, level: Int): Option[Nothing] = None

    protected def filter0(p: ((Any, Nothing)) => Boolean, negate: Boolean, level: Int, buffer: Array[OldHashMap[Any, Nothing]], offset0: Int): OldHashMap[Any, Nothing] = null

    protected def contains0(key: Any, hash: Int, level: Int): Boolean = false

    protected def merge0[V1 >: Nothing](that: OldHashMap[Any, V1], level: Int, merger: Merger[Any, V1]): OldHashMap[Any, V1] = that

    def iterator: Iterator[(Any, Nothing)] = Iterator.empty

    override def foreach[U](f: ((Any, Nothing)) => U): Unit = ()

    override def head: (Any, Nothing) = throw new NoSuchElementException("Empty Map")

    override def headOption: None.type = None

    override def tail: OldHashMap[Any, Nothing] = throw new NoSuchElementException("Empty Map")

    override def last: (Any, Nothing) = throw new NoSuchElementException("Empty Map")

    override def init: OldHashMap[Any, Nothing] = throw new NoSuchElementException("Empty Map")

  }

  final class OldHashMap1[K, +V](private[collection] val key: K, private[collection] val hash: Int, private[collection] val value: V, private[collection] var kv: (K, V@uV)) extends OldHashMap[K, V] {
    override def isEmpty: Boolean = false
    def iterator: Iterator[(K, V)] = Iterator.single(ensurePair)

    def get0(key: K, hash: Int, level: Int): Option[V] =
      if (hash == this.hash && key == this.key) Some(value) else None

    override def size = 1
    override def knownSize: Int = 1
    private[collection] def getKey = key
    private[collection] def getHash = hash
    private[collection] def computeHashFor(k: K) = computeHash(k)

    protected def contains0(key: K, hash: Int, level: Int): Boolean =
      hash == this.hash && key == this.key

    protected def updated0[V1 >: V](key: K, hash: Int, level: Int, value: V1, kv: (K, V1), merger: Merger[K, V1]): OldHashMap[K, V1] =
      if (hash == this.hash && key == this.key ) {
        if (merger eq null) {
          if (this.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this
          else new OldHashMap1(key, hash, value, kv)
        } else {
          val nkv = merger(this.ensurePair, if(kv != null) kv else (key, value))
          new OldHashMap1(nkv._1, hash, nkv._2, nkv)
        }
      } else {
        if (hash != this.hash) {
          // they have different hashes, but may collide at this level - find a level at which they don't
          val that = new OldHashMap1[K, V1](key, hash, value, kv)
          makeHashTrieMap[K,V1](this.hash, this, hash, that, level, 2)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new OldHashMapCollision1(hash, ListMap.empty.updated(this.key,this.value).updated(key,value))
        }
      }

    protected def removed0(key: K, hash: Int, level: Int): OldHashMap[K, V] =
      if (hash == this.hash && key == this.key) OldHashMap.empty[K,V] else this

    protected def filter0(p: ((K, V)) => Boolean, negate: Boolean, level: Int, buffer: Array[OldHashMap[K, V @uV]], offset0: Int): OldHashMap[K, V] =
      if (negate ^ p(ensurePair)) this else null

    override def foreach[U](f: ((K, V)) => U): Unit = f(ensurePair)

    // this method may be called multiple times in a multithreaded environment, but that's ok
    private[OldHashMap] def ensurePair: (K, V) = if (kv ne null) kv else { kv = (key, value); kv }

    protected def merge0[V1 >: V](that: OldHashMap[K, V1], level: Int, merger: Merger[K, V1]): OldHashMap[K, V1] =
      that.updated0(key, hash, level, value, kv, merger.invert)

  }

  private[collection] class OldHashMapCollision1[K, +V](private[collection] val hash: Int, val kvs: ListMap[K, V @uV])
    extends OldHashMap[K, V @uV] {
    // assert(kvs.size > 1)

    override def size: Int = kvs.size
    override def isEmpty: Boolean = false
    protected def get0(key: K, hash: Int, level: Int): Option[V] =
      if (hash == this.hash) kvs.get(key) else None

    protected def contains0(key: K, hash: Int, level: Int): Boolean =
      hash == this.hash && kvs.contains(key)

    protected override def updated0[B1 >: V](key: K, hash: Int, level: Int, value: B1, kv: (K, B1), merger: Merger[K, B1]): OldHashMap[K, B1] =
      if (hash == this.hash) {
        if ((merger eq null) || !kvs.contains(key)) new OldHashMapCollision1(hash, kvs.updated(key, value))
        else new OldHashMapCollision1(hash, kvs + merger((key, kvs(key)), kv))
      } else {
        val that = new OldHashMap1(key, hash, value, kv)
        makeHashTrieMap(this.hash, this, hash, that, level, size + 1)
      }

    override def removed0(key: K, hash: Int, level: Int): OldHashMap[K, V] =
      if (hash == this.hash) {
        val kvs1 = kvs - key
        kvs1.size match {
          case 0 =>
            OldHashMap.empty[K,V]
          case 1 =>
            val kv = kvs1.head
            new OldHashMap1(kv._1,hash,kv._2,kv)
          case x if x == kvs.size =>
            this
          case _ =>
            new OldHashMapCollision1(hash, kvs1)
        }
      } else this

    override protected def filter0(p: ((K, V)) => Boolean, negate: Boolean, level: Int, buffer: Array[OldHashMap[K, V @uV]], offset0: Int): OldHashMap[K, V] = {
      val kvs1 = if (negate) kvs.filterNot(p) else kvs.filter(p)
      kvs1.size match {
        case 0 =>
          null
        case 1 =>
          val kv@(k,v) = kvs1.head
          new OldHashMap1(k, hash, v, kv)
        case x if x == kvs.size =>
          this
        case _ =>
          new OldHashMapCollision1(hash, kvs1)
      }
    }

    protected def merge0[V1 >: V](that: OldHashMap[K, V1], level: Int, merger: Merger[K, V1]): OldHashMap[K, V1] = {
      // this can be made more efficient by passing the entire ListMap at once
      var m = that
      for (p <- kvs) m = m.updated0(p._1, this.hash, level, p._2, p, merger.invert)
      m
    }

    override def iterator: Iterator[(K, V)] = kvs.iterator

    override def foreach[U](f: ((K, V)) => U): Unit = kvs.foreach(f)

    override def split: Seq[OldHashMap[K, V]] = {
      val (x, y) = kvs.splitAt(kvs.size / 2)
      def newhm(lm: ListMap[K, V @uV]) = new OldHashMapCollision1(hash, lm)
      List(newhm(x), newhm(y))
    }

  }

  final class HashTrieMap[K, +V](
    private[collection] val bitmap: Int,
    private[collection] val elems: Array[OldHashMap[K, V @uV]],
    private[collection] val size0: Int
  ) extends OldHashMap[K, V @uV] {

    // assert(Integer.bitCount(bitmap) == elems.length)
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieMap[_,_]]))

    override def size: Int = size0
    override def isEmpty: Boolean = false
    override def knownSize: Int = size
    protected def get0(key: K, hash: Int, level: Int): Option[V] = {
      // Note: this code is duplicated with `contains0`
      val index = (hash >>> level) & 0x1f
      if (bitmap == - 1) {
        elems(index).get0(key, hash, level + 5)
      } else {
        val mask = (1 << index)
        if ((bitmap & mask) != 0) {
          val offset = Integer.bitCount(bitmap & (mask - 1))
          elems(offset).get0(key, hash, level + 5)
        } else {
          None
        }
      }
    }

    protected def contains0(key: K, hash: Int, level: Int): Boolean = {
      // Note: this code is duplicated from `get0`
      val index = (hash >>> level) & 0x1f
      if (bitmap == - 1) {
        elems(index).contains0(key, hash, level + 5)
      } else {
        val mask = (1 << index)
        if ((bitmap & mask) != 0) {
          val offset = Integer.bitCount(bitmap & (mask - 1))
          elems(offset).contains0(key, hash, level + 5)
        } else {
          false
        }
      }
    }

    protected def updated0[V1 >: V](key: K, hash: Int, level: Int, value: V1, kv: (K, V1), merger: Merger[K, V1]): OldHashMap[K, V1] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5, value, kv, merger)
        if(subNew eq sub) this else {
          val elemsNew = new Array[OldHashMap[K,V1]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          new HashTrieMap(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
      } else {
        val elemsNew = new Array[OldHashMap[K,V1]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new OldHashMap1(key, hash, value, kv)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        new HashTrieMap(bitmap | mask, elemsNew, size + 1)
      }
    }

    override def removed0(key: K, hash: Int, level: Int): OldHashMap[K, V] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (subNew eq sub) this
        else if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[OldHashMap[K,V]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // HashSet1 or HashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieMap[_,_]])
              elemsNew(0)
            else
              new HashTrieMap(bitmapNew, elemsNew, sizeNew)
          } else
            OldHashMap.empty[K,V]
        } else if(elems.length == 1 && !subNew.isInstanceOf[HashTrieMap[_,_]]) {
          subNew
        } else {
          val elemsNew = java.util.Arrays.copyOf(elems, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieMap(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    protected def filter0(p: ((K, V)) => Boolean, negate: Boolean, level: Int, buffer: Array[OldHashMap[K, V @uV]], offset0: Int): OldHashMap[K, V] = {
      // current offset
      var offset = offset0
      // result size
      var rs = 0
      // bitmap for kept elems
      var kept = 0
      // loop over all elements
      var i = 0
      while (i < elems.length) {
        val result = elems(i).filter0(p, negate, level + 5, buffer, offset)
        if (result ne null) {
          buffer(offset) = result
          offset += 1
          // add the result size
          rs += result.size
          // mark the bit i as kept
          kept |= (1 << i)
        }
        i += 1
      }
      if (offset == offset0) {
        // empty
        null
      } else if (rs == size0) {
        // unchanged
        this
      } else if (offset == offset0 + 1 && !buffer(offset0).isInstanceOf[HashTrieMap[K, V]]) {
        // leaf
        buffer(offset0)
      } else {
        // we have to return a HashTrieMap
        val length = offset - offset0
        val elems1 = new Array[OldHashMap[K, V]](length)
        System.arraycopy(buffer, offset0, elems1, 0, length)
        val bitmap1 = if (length == elems.length) {
          // we can reuse the original bitmap
          bitmap
        } else {
          // calculate new bitmap by keeping just bits in the kept bitmask
          keepBits(bitmap, kept)
        }
        new HashTrieMap(bitmap1, elems1, rs)
      }
    }

    override def iterator: Iterator[(K, V)] = new TrieIterator[(K, V)](elems.asInstanceOf[Array[Iterable[(K, V)]]]) {
      final override def getElem(cc: AnyRef): (K, V) = cc.asInstanceOf[OldHashMap1[K, V]].ensurePair
    }

    override def foreach[U](f: ((K, V)) => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }

    private def posOf(n: Int, bm: Int) = {
      var left = n
      var i = -1
      var b = bm
      while (left >= 0) {
        i += 1
        if ((b & 1) != 0) left -= 1
        b = b >>> 1
      }
      i
    }

    override def split: Seq[OldHashMap[K, V]] = if (size == 1) Seq(this) else {
      val nodesize = Integer.bitCount(bitmap)
      if (nodesize > 1) {
        val splitpoint = nodesize / 2
        val bitsplitpoint = posOf(nodesize / 2, bitmap)
        val bm1 = bitmap & (-1 << bitsplitpoint)
        val bm2 = bitmap & (-1 >>> (32 - bitsplitpoint))

        val (e1, e2) = elems.splitAt(splitpoint)
        val hm1 = new HashTrieMap(bm1, e1, e1.foldLeft(0)(_ + _.size))
        val hm2 = new HashTrieMap(bm2, e2, e2.foldLeft(0)(_ + _.size))

        List(hm1, hm2)
      } else elems(0).split
    }

    protected def merge0[V1 >: V](that: OldHashMap[K, V1], level: Int, merger: Merger[K, V1]): OldHashMap[K, V1] = that match {
      case hm: OldHashMap1[_, _] =>
        this.updated0(hm.key, hm.hash, level, hm.value.asInstanceOf[V1], hm.kv, merger)
      case hm: HashTrieMap[_, _] =>
        val that = hm.asInstanceOf[HashTrieMap[K, V1]]
        val thiselems = this.elems
        val thatelems = that.elems
        var thisbm = this.bitmap
        var thatbm = that.bitmap

        // determine the necessary size for the array
        val subcount = Integer.bitCount(thisbm | thatbm)

        // construct a new array of appropriate size
        val merged = new Array[OldHashMap[K, V1]](subcount)

        // run through both bitmaps and add elements to it
        var i = 0
        var thisi = 0
        var thati = 0
        var totalelems = 0
        while (i < subcount) {
          val thislsb = thisbm ^ (thisbm & (thisbm - 1))
          val thatlsb = thatbm ^ (thatbm & (thatbm - 1))

          // collision
          if (thislsb == thatlsb) {
            val m = thiselems(thisi).merge0(thatelems(thati), level + 5, merger)
            totalelems += m.size
            merged(i) = m
            thisbm = thisbm & ~thislsb
            thatbm = thatbm & ~thatlsb
            thati += 1
            thisi += 1
          } else {
            if (Integer.compareUnsigned(thislsb - 1, thatlsb - 1) < 0) {
              val m = thiselems(thisi)
              totalelems += m.size
              merged(i) = m
              thisbm = thisbm & ~thislsb
              thisi += 1
            }
            else {
              val m = thatelems(thati)
              totalelems += m.size
              merged(i) = m
              thatbm = thatbm & ~thatlsb
              thati += 1
            }
          }
          i += 1
        }

        new HashTrieMap[K, V1](this.bitmap | that.bitmap, merged, totalelems)
      case hm: OldHashMapCollision1[_, _] => that.merge0(this, level, merger.invert)
      case hm: OldHashMap[_, _] => this
    }
  }

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}
