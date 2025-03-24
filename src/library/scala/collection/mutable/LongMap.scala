/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package mutable

import scala.collection.generic.DefaultSerializationProxy
import scala.language.implicitConversions

/** This class implements mutable maps with `Long` keys based on a hash table with open addressing.
  *
  *  Basic map operations on single entries, including `contains` and `get`,
  *  are typically substantially faster with `LongMap` than [[HashMap]].  Methods
  *  that act on the whole map,  including `foreach` and `map` are not in
  *  general expected to be faster than with a generic map, save for those
  *  that take particular advantage of the internal structure of the map:
  *  `foreachKey`, `foreachValue`, `mapValuesNow`, and `transformValues`.
  *
  *  Maps with open addressing may become less efficient at lookup after
  *  repeated addition/removal of elements.  Although `LongMap` makes a
  *  decent attempt to remain efficient regardless,  calling `repack`
  *  on a map that will no longer have elements removed but will be
  *  used heavily may save both time and storage space.
  *
  *  This map is not intended to contain more than 2^29 entries (approximately
  *  500 million).  The maximum capacity is 2^30, but performance will degrade
  *  rapidly as 2^30 is approached.
  *
  */
final class LongMap[V] private[collection] (defaultEntry: Long => V, initialBufferSize: Int, initBlank: Boolean)
  extends AbstractMap[Long, V]
    with MapOps[Long, V, Map, LongMap[V]]
    with StrictOptimizedIterableOps[(Long, V), Iterable, LongMap[V]]
    with Serializable {
  import LongMap._

  def this() = this(LongMap.exceptionDefault, 16, initBlank = true)

  // TODO: override clear() with an optimization more tailored for efficiency.
  override protected def fromSpecific(coll: scala.collection.IterableOnce[(Long, V)]): LongMap[V] = {
    //TODO should this be the default implementation of this method in StrictOptimizedIterableOps?
    val b = newSpecificBuilder
    b.sizeHint(coll)
    b.addAll(coll)
    b.result()
  }
  override protected def newSpecificBuilder: Builder[(Long, V),LongMap[V]] = new GrowableBuilder(LongMap.empty[V])

  /** Creates a new `LongMap` that returns default values according to a supplied key-value mapping. */
  def this(defaultEntry: Long => V) = this(defaultEntry, 16, initBlank = true)

  /** Creates a new `LongMap` with an initial buffer of specified size.
    *
    *  A LongMap can typically contain half as many elements as its buffer size
    *  before it requires resizing.
    */
  def this(initialBufferSize: Int) = this(LongMap.exceptionDefault, initialBufferSize, initBlank = true)

  /** Creates a new `LongMap` with specified default values and initial buffer size. */
  def this(defaultEntry: Long => V,  initialBufferSize: Int) = this(defaultEntry,  initialBufferSize,  initBlank = true)

  private[this] var mask = 0
  private[this] var extraKeys: Int = 0
  private[this] var zeroValue: AnyRef = null
  private[this] var minValue: AnyRef = null
  private[this] var _size = 0
  private[this] var _vacant = 0
  private[this] var _keys: Array[Long] = null
  private[this] var _values: Array[AnyRef] = null

  if (initBlank) defaultInitialize(initialBufferSize)

  private[this] def defaultInitialize(n: Int) = {
    mask =
      if (n<0) 0x7
      else (((1 << (32 - java.lang.Integer.numberOfLeadingZeros(n-1))) - 1) & 0x3FFFFFFF) | 0x7
    _keys = new Array[Long](mask+1)
    _values = new Array[AnyRef](mask+1)
  }

  private[collection] def initializeTo(
                                        m: Int, ek: Int, zv: AnyRef, mv: AnyRef, sz: Int, vc: Int, kz: Array[Long], vz: Array[AnyRef]
                                      ): Unit = {
    mask = m; extraKeys = ek; zeroValue = zv; minValue = mv; _size = sz; _vacant = vc; _keys = kz; _values = vz
  }

  override def size: Int = _size + (extraKeys+1)/2
  override def knownSize: Int = size
  override def isEmpty: Boolean = size == 0
  override def empty: LongMap[V] = new LongMap()

  private def imbalanced: Boolean =
    (_size + _vacant) > 0.5*mask || _vacant > _size

  private def toIndex(k: Long): Int = {
    // Part of the MurmurHash3 32 bit finalizer
    val h = ((k ^ (k >>> 32)) & 0xFFFFFFFFL).toInt
    val x = (h ^ (h >>> 16)) * 0x85EBCA6B
    (x ^ (x >>> 13)) & mask
  }

  private def seekEmpty(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    while (_keys(e) != 0) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
    e
  }

  private def seekEntry(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q==k) return e; q != 0}) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
    e | MissingBit
  }

  private def seekEntryOrOpen(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q==k) return e; q+q != 0}) {
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    if (q == 0) return e | MissingBit
    val o = e | MissVacant
    while ({ q = _keys(e); if (q==k) return e; q != 0}) {
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    o
  }

  override def contains(key: Long): Boolean = {
    if (key == -key) (((key>>>63).toInt+1) & extraKeys) != 0
    else seekEntry(key) >= 0
  }

  override def get(key: Long): Option[V] = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) None
      else if (key == 0) Some(zeroValue.asInstanceOf[V])
      else Some(minValue.asInstanceOf[V])
    }
    else {
      val i = seekEntry(key)
      if (i < 0) None else Some(_values(i).asInstanceOf[V])
    }
  }

  override def getOrElse[V1 >: V](key: Long, default: => V1): V1 = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) default
      else if (key == 0) zeroValue.asInstanceOf[V1]
      else minValue.asInstanceOf[V1]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) default else _values(i).asInstanceOf[V1]
    }
  }

  override def getOrElseUpdate(key: Long, defaultValue: => V): V = {
    if (key == -key) {
      val kbits = (key>>>63).toInt + 1
      if ((kbits & extraKeys) == 0) {
        val value = defaultValue
        extraKeys |= kbits
        if (key == 0) zeroValue = value.asInstanceOf[AnyRef]
        else minValue = value.asInstanceOf[AnyRef]
        value
      }
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      var i = seekEntryOrOpen(key)
      if (i < 0) {
        val value = {
          val oks = _keys
          val j = i & IndexMask
          val ok = oks(j)
          val ans = defaultValue
          // Evaluating `defaultValue` may change the map
          //   - repack: the array is different
          //   - element added at `j`: since `i < 0`, the key was missing and `ok` is either 0 or MinValue.
          //     If `defaultValue` added an element at `j` then `_keys(j)` must be different now.
          //     (`_keys` never contains 0 or MinValue.)
          if (oks.ne(_keys) || ok != _keys(j)) {
            i = seekEntryOrOpen(key)
            if (i >= 0) _size -= 1
          }
          ans
        }
        _size += 1
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
        value
      }
      else _values(i).asInstanceOf[V]
    }
  }

  /** Retrieves the value associated with a key, or the default for that type if none exists
    *  (null for AnyRef, 0 for floats and integers).
    *
    *  Note: this is the fastest way to retrieve a value that may or
    *  may not exist, if the default null/zero is acceptable.  For key/value
    *  pairs that do exist,  `apply` (i.e. `map(key)`) is equally fast.
    */
  def getOrNull(key: Long): V = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) null.asInstanceOf[V]
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) null.asInstanceOf[V] else _values(i).asInstanceOf[V]
    }
  }

  /** Retrieves the value associated with a key.
    *  If the key does not exist in the map, the `defaultEntry` for that key
    *  will be returned instead.
    */
  override def apply(key: Long): V = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) defaultEntry(key)
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) defaultEntry(key) else _values(i).asInstanceOf[V]
    }
  }

  /** The user-supplied default value for the key.  Throws an exception
    *  if no other default behavior was specified.
    */
  override def default(key: Long) = defaultEntry(key)

  private def repack(newMask: Int): Unit = {
    val ok = _keys
    val ov = _values
    mask = newMask
    _keys = new Array[Long](mask+1)
    _values = new Array[AnyRef](mask+1)
    _vacant = 0
    var i = 0
    while (i < ok.length) {
      val k = ok(i)
      if (k != -k) {
        val j = seekEmpty(k)
        _keys(j) = k
        _values(j) = ov(i)
      }
      i += 1
    }
  }

  /** Repacks the contents of this `LongMap` for maximum efficiency of lookup.
   *
   *  For maps that undergo a complex creation process with both addition and
   *  removal of keys, and then are used heavily with no further removal of
   *  elements, calling `repack` after the end of the creation can result in
   *  improved performance.  Repacking takes time proportional to the number
   *  of entries in the map.
   */
  def repack(): Unit = repack(repackMask(mask, _size = _size, _vacant = _vacant))

  override def put(key: Long, value: V): Option[V] = {
    if (key == -key) {
      if (key == 0) {
        val ans = if ((extraKeys&1) == 1) Some(zeroValue.asInstanceOf[V]) else None
        zeroValue = value.asInstanceOf[AnyRef]
        extraKeys |= 1
        ans
      }
      else {
        val ans = if ((extraKeys&2) == 1) Some(minValue.asInstanceOf[V]) else None
        minValue = value.asInstanceOf[AnyRef]
        extraKeys |= 2
        ans
      }
    }
    else {
      val i = seekEntryOrOpen(key)
      if (i < 0) {
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        _size += 1
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
        None
      }
      else {
        val ans = Some(_values(i).asInstanceOf[V])
        _keys(i) = key
        _values(i) = value.asInstanceOf[AnyRef]
        ans
      }
    }
  }

  /** Updates the map to include a new key-value pair.
    *
    *  This is the fastest way to add an entry to a `LongMap`.
    */
  override def update(key: Long, value: V): Unit = {
    if (key == -key) {
      if (key == 0) {
        zeroValue = value.asInstanceOf[AnyRef]
        extraKeys |= 1
      }
      else {
        minValue = value.asInstanceOf[AnyRef]
        extraKeys |= 2
      }
    }
    else {
      val i = seekEntryOrOpen(key)
      if (i < 0) {
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        _size += 1
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
      }
      else {
        _keys(i) = key
        _values(i) = value.asInstanceOf[AnyRef]
      }
    }
  }

  /** Adds a new key/value pair to this map and returns the map. */
  @deprecated("Use `addOne` or `update` instead; infix operations with an operand of multiple args will be deprecated", "2.13.3")
  def +=(key: Long, value: V): this.type = { update(key, value); this }

  /** Adds a new key/value pair to this map and returns the map. */
  @inline final def addOne(key: Long, value: V): this.type = { update(key, value); this }

  @inline override final def addOne(kv: (Long, V)): this.type = { update(kv._1, kv._2); this }

  def subtractOne(key: Long): this.type = {
    if (key == -key) {
      if (key == 0L) {
        extraKeys &= 0x2
        zeroValue = null
      }
      else {
        extraKeys &= 0x1
        minValue = null
      }
    }
    else {
      val i = seekEntry(key)
      if (i >= 0) {
        _size -= 1
        _vacant += 1
        _keys(i) = Long.MinValue
        _values(i) = null
      }
    }
    this
  }

  def iterator: Iterator[(Long, V)] = new AbstractIterator[(Long, V)] {
    private[this] val kz = _keys
    private[this] val vz = _values

    private[this] var nextPair: (Long, V) =
      if (extraKeys==0) null
      else if ((extraKeys&1)==1) (0L, zeroValue.asInstanceOf[V])
      else (Long.MinValue, minValue.asInstanceOf[V])

    private[this] var anotherPair: (Long, V) =
      if (extraKeys==3) (Long.MinValue, minValue.asInstanceOf[V])
      else null

    private[this] var index = 0

    def hasNext: Boolean = nextPair != null || (index < kz.length && {
      var q = kz(index)
      while (q == -q) {
        index += 1
        if (index >= kz.length) return false
        q = kz(index)
      }
      nextPair = (kz(index), vz(index).asInstanceOf[V])
      index += 1
      true
    })
    def next() = {
      if (nextPair == null && !hasNext) throw new NoSuchElementException("next")
      val ans = nextPair
      if (anotherPair != null) {
        nextPair = anotherPair
        anotherPair = null
      }
      else nextPair = null
      ans
    }
  }

  // TODO PERF override these for efficiency. See immutable.LongMap for how to organize the code.
  override def keysIterator: Iterator[Long] = super.keysIterator
  override def valuesIterator: Iterator[V] = super.valuesIterator

  override def foreach[U](f: ((Long,V)) => U): Unit = {
    if ((extraKeys & 1) == 1) f((0L, zeroValue.asInstanceOf[V]))
    if ((extraKeys & 2) == 2) f((Long.MinValue, minValue.asInstanceOf[V]))
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f((k, _values(i).asInstanceOf[V]))
      }
      i += 1
    }
  }

  override def foreachEntry[U](f: (Long,V) => U): Unit = {
    if ((extraKeys & 1) == 1) f(0L, zeroValue.asInstanceOf[V])
    if ((extraKeys & 2) == 2) f(Long.MinValue, minValue.asInstanceOf[V])
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(k, _values(i).asInstanceOf[V])
      }
      i += 1
    }
  }

  override def clone(): LongMap[V] = {
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = java.util.Arrays.copyOf(_values,  _values.length)
    val lm = new LongMap[V](defaultEntry, 1, initBlank = false)
    lm.initializeTo(mask, extraKeys, zeroValue, minValue, _size, _vacant, kz,  vz)
    lm
  }

  @deprecated("Consider requiring an immutable Map or fall back to Map.concat", "2.13.0")
  override def +[V1 >: V](kv: (Long, V1)): LongMap[V1] = {
    val lm = clone().asInstanceOf[LongMap[V1]]
    lm += kv
    lm
  }

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (Long, V1), elem2: (Long, V1), elems: (Long, V1)*): LongMap[V1] = {
    val m = this + elem1 + elem2
    if(elems.isEmpty) m else m.concat(elems)
  }

  override def concat[V1 >: V](xs: scala.collection.IterableOnce[(Long, V1)]): LongMap[V1] = {
    val lm = clone().asInstanceOf[LongMap[V1]]
    xs.iterator.foreach(kv => lm += kv)
    lm
  }

  override def ++ [V1 >: V](xs: scala.collection.IterableOnce[(Long, V1)]): LongMap[V1] = concat(xs)

  @deprecated("Use m.clone().addOne(k,v) instead of m.updated(k, v)", "2.13.0")
  override def updated[V1 >: V](key: Long, value: V1): LongMap[V1] =
    clone().asInstanceOf[LongMap[V1]].addOne(key, value)

  /** Applies a function to all keys of this map. */
  def foreachKey[A](f: Long => A): Unit = {
    if ((extraKeys & 1) == 1) f(0L)
    if ((extraKeys & 2) == 2) f(Long.MinValue)
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(k)
      }
      i += 1
    }
  }

  /** Applies a function to all values of this map. */
  def foreachValue[A](f: V => A): Unit = {
    if ((extraKeys & 1) == 1) f(zeroValue.asInstanceOf[V])
    if ((extraKeys & 2) == 2) f(minValue.asInstanceOf[V])
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(_values(i).asInstanceOf[V])
      }
      i += 1
    }
  }

  /** Creates a new `LongMap` with different values.
    *  Unlike `mapValues`, this method generates a new
    *  collection immediately.
    */
  def mapValuesNow[V1](f: V => V1): LongMap[V1] = {
    val zv = if ((extraKeys & 1) == 1) f(zeroValue.asInstanceOf[V]).asInstanceOf[AnyRef] else null
    val mv = if ((extraKeys & 2) == 2) f(minValue.asInstanceOf[V]).asInstanceOf[AnyRef] else null
    val lm = new LongMap[V1](LongMap.exceptionDefault,  1,  initBlank = false)
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = new Array[AnyRef](_values.length)
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        vz(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
      }
      i += 1
    }
    lm.initializeTo(mask, extraKeys, zv, mv, _size, _vacant, kz, vz)
    lm
  }

  /** Applies a transformation function to all values stored in this map.
    *  Note: the default, if any,  is not transformed.
    */
  @deprecated("Use transformValuesInPlace instead of transformValues", "2.13.0")
  @`inline` final def transformValues(f: V => V): this.type = transformValuesInPlace(f)

  /** Applies a transformation function to all values stored in this map.
    *  Note: the default, if any,  is not transformed.
    */
  def transformValuesInPlace(f: V => V): this.type = {
    if ((extraKeys & 1) == 1) zeroValue = f(zeroValue.asInstanceOf[V]).asInstanceOf[AnyRef]
    if ((extraKeys & 2) == 2) minValue = f(minValue.asInstanceOf[V]).asInstanceOf[AnyRef]
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        _values(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
      }
      i += 1
    }
    this
  }

  /** An overload of `map` which produces a `LongMap`.
   *
   *  @param f the mapping function
   */
  def map[V2](f: ((Long, V)) => (Long, V2)): LongMap[V2] = LongMap.from(new View.Map(coll, f))

  /** An overload of `flatMap` which produces a `LongMap`.
   *
   *  @param f the mapping function
   */
  def flatMap[V2](f: ((Long, V)) => IterableOnce[(Long, V2)]): LongMap[V2] = LongMap.from(new View.FlatMap(coll, f))

  /** An overload of `collect` which produces a `LongMap`.
   *
   *  @param pf the mapping function
   */
  def collect[V2](pf: PartialFunction[(Long, V), (Long, V2)]): LongMap[V2] =
    strictOptimizedCollect(LongMap.newBuilder[V2], pf)

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(LongMap.toFactory[V](LongMap), this)

  override protected[this] def className = "LongMap"
}

object LongMap {
  private final val IndexMask  = 0x3FFF_FFFF
  private final val MissingBit = 0x8000_0000
  private final val VacantBit  = 0x4000_0000
  private final val MissVacant = 0xC000_0000

  private val exceptionDefault: Long => Nothing = (k: Long) => throw new NoSuchElementException(k.toString)

  /** A builder for instances of `LongMap`.
    *
    *  This builder can be reused to create multiple instances.
    */
  final class LongMapBuilder[V] extends ReusableBuilder[(Long, V), LongMap[V]] {
    private[collection] var elems: LongMap[V] = new LongMap[V]
    override def addOne(entry: (Long, V)): this.type = {
      elems += entry
      this
    }
    def clear(): Unit = elems = new LongMap[V]
    def result(): LongMap[V] = elems
    override def knownSize: Int = elems.knownSize
  }

  /** Creates a new `LongMap` with zero or more key/value pairs. */
  def apply[V](elems: (Long, V)*): LongMap[V] = buildFromIterableOnce(elems)

  private def buildFromIterableOnce[V](elems: IterableOnce[(Long, V)]): LongMap[V] = {
    var sz = elems.knownSize
    if(sz < 0) sz = 4
    val lm = new LongMap[V](sz * 2)
    elems.iterator.foreach{ case (k,v) => lm(k) = v }
    if (lm.size < (sz>>3)) lm.repack()
    lm
  }

  /** Creates a new empty `LongMap`. */
  def empty[V]: LongMap[V] = new LongMap[V]

  /** Creates a new empty `LongMap` with the supplied default */
  def withDefault[V](default: Long => V): LongMap[V] = new LongMap[V](default)

  /** Creates a new `LongMap` from an existing source collection. A source collection
    * which is already a `LongMap` gets cloned.
    *
    * @param source Source collection
    * @tparam A the type of the collection’s elements
    * @return a new `LongMap` with the elements of `source`
    */
  def from[V](source: IterableOnce[(Long, V)]): LongMap[V] = source match {
    case source: LongMap[_] => source.clone().asInstanceOf[LongMap[V]]
    case _ => buildFromIterableOnce(source)
  }

  def newBuilder[V]: ReusableBuilder[(Long, V), LongMap[V]] = new LongMapBuilder[V]

  /** Creates a new `LongMap` from arrays of keys and values.
    *  Equivalent to but more efficient than `LongMap((keys zip values): _*)`.
    */
  def fromZip[V](keys: Array[Long], values: Array[V]): LongMap[V] = {
    val sz = math.min(keys.length, values.length)
    val lm = new LongMap[V](sz * 2)
    var i = 0
    while (i < sz) { lm(keys(i)) = values(i); i += 1 }
    if (lm.size < (sz>>3)) lm.repack()
    lm
  }

  /** Creates a new `LongMap` from keys and values.
    *  Equivalent to but more efficient than `LongMap((keys zip values): _*)`.
    */
  def fromZip[V](keys: scala.collection.Iterable[Long], values: scala.collection.Iterable[V]): LongMap[V] = {
    val sz = math.min(keys.size, values.size)
    val lm = new LongMap[V](sz * 2)
    val ki = keys.iterator
    val vi = values.iterator
    while (ki.hasNext && vi.hasNext) lm(ki.next()) = vi.next()
    if (lm.size < (sz >> 3)) lm.repack()
    lm
  }

  implicit def toFactory[V](dummy: LongMap.type): Factory[(Long, V), LongMap[V]] = ToFactory.asInstanceOf[Factory[(Long, V), LongMap[V]]]

  @SerialVersionUID(3L)
  private[this] object ToFactory extends Factory[(Long, AnyRef), LongMap[AnyRef]] with Serializable {
    def fromSpecific(it: IterableOnce[(Long, AnyRef)]): LongMap[AnyRef] = LongMap.from[AnyRef](it)
    def newBuilder: Builder[(Long, AnyRef), LongMap[AnyRef]] = LongMap.newBuilder[AnyRef]
  }

  implicit def toBuildFrom[V](factory: LongMap.type): BuildFrom[Any, (Long, V), LongMap[V]] = ToBuildFrom.asInstanceOf[BuildFrom[Any, (Long, V), LongMap[V]]]
  private object ToBuildFrom extends BuildFrom[Any, (Long, AnyRef), LongMap[AnyRef]] {
    def fromSpecific(from: Any)(it: IterableOnce[(Long, AnyRef)]) = LongMap.from(it)
    def newBuilder(from: Any): ReusableBuilder[(Long, AnyRef), LongMap[AnyRef]] = LongMap.newBuilder[AnyRef]
  }

  implicit def iterableFactory[V]: Factory[(Long, V), LongMap[V]] = toFactory(this)
  implicit def buildFromLongMap[V]: BuildFrom[LongMap[_], (Long, V), LongMap[V]] = toBuildFrom(this)

  private def repackMask(mask: Int, _size: Int, _vacant: Int): Int = {
    var m = mask
    if (_size + _vacant >= 0.5*mask && !(_vacant > 0.2*mask)) m = ((m << 1) + 1) & IndexMask
    while (m > 8 && _size < (m >>> 3)) m = m >>> 1
    m /*.ensuring(_size <= _ + 1)*/
  }
}
