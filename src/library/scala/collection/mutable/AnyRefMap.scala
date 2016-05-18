package scala
package collection
package mutable

import generic.CanBuildFrom

/** This class implements mutable maps with `AnyRef` keys based on a hash table with open addressing.
 *
 *  Basic map operations on single entries, including `contains` and `get`,
 *  are typically significantly faster with `AnyRefMap` than [[HashMap]].
 *  Note that numbers and characters are not handled specially in AnyRefMap;
 *  only plain `equals` and `hashCode` are used in comparisons.
 *
 *  Methods that traverse or regenerate the map, including `foreach` and `map`,
 *  are not in general faster than with `HashMap`.  The methods `foreachKey`,
 *  `foreachValue`, `mapValuesNow`, and `transformValues` are, however, faster
 *  than alternative ways to achieve the same functionality.
 *
 *  Maps with open addressing may become less efficient at lookup after
 *  repeated addition/removal of elements.  Although `AnyRefMap` makes a
 *  decent attempt to remain efficient regardless,  calling `repack`
 *  on a map that will no longer have elements removed but will be
 *  used heavily may save both time and storage space.
 *
 *  This map is not intended to contain more than 2^29^ entries (approximately
 *  500 million).  The maximum capacity is 2^30^, but performance will degrade
 *  rapidly as 2^30^ is approached.
 *
 */
final class AnyRefMap[K <: AnyRef, V] private[collection] (defaultEntry: K => V, initialBufferSize: Int, initBlank: Boolean)
extends AbstractMap[K, V]
   with Map[K, V]
   with MapLike[K, V, AnyRefMap[K, V]]
{
  import AnyRefMap._
  def this() = this(AnyRefMap.exceptionDefault, 16, true)

  /** Creates a new `AnyRefMap` that returns default values according to a supplied key-value mapping. */
  def this(defaultEntry: K => V) = this(defaultEntry, 16, true)

  /** Creates a new `AnyRefMap` with an initial buffer of specified size.
   *
   *  An `AnyRefMap` can typically contain half as many elements as its buffer size
   *  before it requires resizing.
   */
  def this(initialBufferSize: Int) = this(AnyRefMap.exceptionDefault, initialBufferSize, true)

  /** Creates a new `AnyRefMap` with specified default values and initial buffer size. */
  def this(defaultEntry: K => V, initialBufferSize: Int) = this(defaultEntry, initialBufferSize, true)

  private[this] var mask = 0
  private[this] var _size = 0
  private[this] var _vacant = 0
  private[this] var _hashes: Array[Int] = null
  private[this] var _keys: Array[AnyRef] = null
  private[this] var _values: Array[AnyRef] = null

  if (initBlank) defaultInitialize(initialBufferSize)

  private[this] def defaultInitialize(n: Int) {
    mask =
      if (n<0) 0x7
      else (((1 << (32 - java.lang.Integer.numberOfLeadingZeros(n-1))) - 1) & 0x3FFFFFFF) | 0x7
    _hashes = new Array[Int](mask+1)
    _keys = new Array[AnyRef](mask+1)
    _values = new Array[AnyRef](mask+1)
  }

  private[collection] def initializeTo(
    m: Int, sz: Int, vc: Int, hz: Array[Int], kz: Array[AnyRef], vz: Array[AnyRef]
  ) {
    mask = m; _size = sz; _vacant = vc; _hashes = hz; _keys = kz; _values = vz
  }

  override def size: Int = _size
  override def empty: AnyRefMap[K,V] = new AnyRefMap(defaultEntry)

  private def imbalanced: Boolean =
    (_size + _vacant) > 0.5*mask || _vacant > _size

  private def hashOf(key: K): Int = {
    if (key eq null) 0x41081989
    else {
      val h = key.hashCode
      // Part of the MurmurHash3 32 bit finalizer
      val i = (h ^ (h >>> 16)) * 0x85EBCA6B
      val j = (i ^ (i >>> 13))
      if (j==0) 0x41081989 else j & 0x7FFFFFFF
    }
  }

  private def seekEntry(h: Int, k: AnyRef): Int = {
    var e = h & mask
    var x = 0
    var g = 0
    while ({ g = _hashes(e); g != 0}) {
      if (g == h && { val q = _keys(e); (q eq k) || ((q ne null) && (q equals k)) }) return e
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    e | MissingBit
  }

  private def seekEntryOrOpen(h: Int, k: AnyRef): Int = {
    var e = h & mask
    var x = 0
    var g = 0
    var o = -1
    while ({ g = _hashes(e); g != 0}) {
      if (g == h && { val q = _keys(e); (q eq k) || ((q ne null) && (q equals k)) }) return e
      else if (o == -1 && g+g == 0) o = e
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    if (o >= 0) o | MissVacant else e | MissingBit
  }

  override def contains(key: K): Boolean = seekEntry(hashOf(key), key) >= 0

  override def get(key: K): Option[V] = {
    val i = seekEntry(hashOf(key), key)
    if (i < 0) None else Some(_values(i).asInstanceOf[V])
  }

  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val i = seekEntry(hashOf(key), key)
    if (i < 0) default else _values(i).asInstanceOf[V]
  }

  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    val h = hashOf(key)
    var i = seekEntryOrOpen(h, key)
    if (i < 0) {
      // It is possible that the default value computation was side-effecting
      // Our hash table may have resized or even contain what we want now
      // (but if it does, we'll replace it)
      val value = {
        val oh = _hashes
        val ans = defaultValue
        if (oh ne _hashes) {
          i = seekEntryOrOpen(h, key)
          if (i >= 0) _size -= 1
        }
        ans
      }
      _size += 1
      val j = i & IndexMask
      _hashes(j) = h
      _keys(j) = key.asInstanceOf[AnyRef]
      _values(j) = value.asInstanceOf[AnyRef]
      if ((i & VacantBit) != 0) _vacant -= 1
      else if (imbalanced) repack()
      value
    }
    else _values(i).asInstanceOf[V]
  }

  /** Retrieves the value associated with a key, or the default for that type if none exists
   *  (null for AnyRef, 0 for floats and integers).
   *
   *  Note: this is the fastest way to retrieve a value that may or
   *  may not exist, if the default null/zero is acceptable.  For key/value
   *  pairs that do exist, `apply` (i.e. `map(key)`) is equally fast.
   */
  def getOrNull(key: K): V = {
    val i = seekEntry(hashOf(key), key)
    (if (i < 0) null else _values(i)).asInstanceOf[V]
  }

  /** Retrieves the value associated with a key.
   *  If the key does not exist in the map, the `defaultEntry` for that key
   *  will be returned instead; an exception will be thrown if no
   *  `defaultEntry` was supplied.
   */
  override def apply(key: K): V = {
    val i = seekEntry(hashOf(key), key)
    if (i < 0) defaultEntry(key) else _values(i).asInstanceOf[V]
  }

  /** Defers to defaultEntry to find a default value for the key.  Throws an
   *  exception if no other default behavior was specified.
   */
  override def default(key: K) = defaultEntry(key)

  private def repack(newMask: Int) {
    val oh = _hashes
    val ok = _keys
    val ov = _values
    mask = newMask
    _hashes = new Array[Int](mask+1)
    _keys = new Array[AnyRef](mask+1)
    _values = new Array[AnyRef](mask+1)
    _vacant = 0
    var i = 0
    while (i < oh.length) {
      val h = oh(i)
      if (h+h != 0) {
        var e = h & mask
        var x = 0
        while (_hashes(e) != 0) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
        _hashes(e) = h
        _keys(e) = ok(i)
        _values(e) = ov(i)
      }
      i += 1
    }
  }

  /** Repacks the contents of this `AnyRefMap` for maximum efficiency of lookup.
   *
   *  For maps that undergo a complex creation process with both addition and
   *  removal of keys, and then are used heavily with no further removal of
   *  elements, calling `repack` after the end of the creation can result in
   *  improved performance.  Repacking takes time proportional to the number
   *  of entries in the map.
   */
  def repack() {
    var m = mask
    if (_size + _vacant >= 0.5*mask && !(_vacant > 0.2*mask)) m = ((m << 1) + 1) & IndexMask
    while (m > 8 && 8*_size < m) m = m >>> 1
    repack(m)
  }

  override def put(key: K, value: V): Option[V] = {
    val h = hashOf(key)
    val k = key
    val i = seekEntryOrOpen(h, k)
    if (i < 0) {
      val j = i & IndexMask
      _hashes(j) = h
      _keys(j) = k
      _values(j) = value.asInstanceOf[AnyRef]
      _size += 1
      if ((i & VacantBit) != 0) _vacant -= 1
      else if (imbalanced) repack()
      None
    }
    else {
      val ans = Some(_values(i).asInstanceOf[V])
      _hashes(i) = h
      _keys(i) = k
      _values(i) = value.asInstanceOf[AnyRef]
      ans
    }
  }

  /** Updates the map to include a new key-value pair.
   *
   *  This is the fastest way to add an entry to an `AnyRefMap`.
   */
  override def update(key: K, value: V): Unit = {
    val h = hashOf(key)
    val k = key
    val i = seekEntryOrOpen(h, k)
    if (i < 0) {
      val j = i & IndexMask
      _hashes(j) = h
      _keys(j) = k
      _values(j) = value.asInstanceOf[AnyRef]
      _size += 1
      if ((i & VacantBit) != 0) _vacant -= 1
      else if (imbalanced) repack()
    }
    else {
      _hashes(i) = h
      _keys(i) = k
      _values(i) = value.asInstanceOf[AnyRef]
    }
  }

  /** Adds a new key/value pair to this map and returns the map. */
  def +=(key: K, value: V): this.type = { update(key, value); this }

  def +=(kv: (K, V)): this.type = { update(kv._1, kv._2); this }

  def -=(key: K): this.type = {
    val i = seekEntry(hashOf(key), key)
    if (i >= 0) {
      _size -= 1
      _vacant += 1
      _hashes(i) = Int.MinValue
      _keys(i) = null
      _values(i) = null
    }
    this
  }

  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    private[this] val hz = _hashes
    private[this] val kz = _keys
    private[this] val vz = _values

    private[this] var index = 0

    def hasNext: Boolean = index<hz.length && {
      var h = hz(index)
      while (h+h == 0) {
        index += 1
        if (index >= hz.length) return false
        h = hz(index)
      }
      true
    }

    def next: (K, V) = {
      if (hasNext) {
        val ans = (kz(index).asInstanceOf[K], vz(index).asInstanceOf[V])
        index += 1
        ans
      }
      else throw new NoSuchElementException("next")
    }
  }

  override def foreach[U](f: ((K,V)) => U) {
    var i = 0
    var e = _size
    while (e > 0) {
      while(i < _hashes.length && { val h = _hashes(i); h+h == 0 && i < _hashes.length}) i += 1
      if (i < _hashes.length) {
        f((_keys(i).asInstanceOf[K], _values(i).asInstanceOf[V]))
        i += 1
        e -= 1
      }
      else return
    }
  }

  override def clone(): AnyRefMap[K, V] = {
    val hz = java.util.Arrays.copyOf(_hashes, _hashes.length)
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = java.util.Arrays.copyOf(_values,  _values.length)
    val arm = new AnyRefMap[K, V](defaultEntry, 1, false)
    arm.initializeTo(mask, _size, _vacant, hz, kz,  vz)
    arm
  }

  private[this] def foreachElement[A,B](elems: Array[AnyRef], f: A => B) {
    var i,j = 0
    while (i < _hashes.length & j < _size) {
      val h = _hashes(i)
      if (h+h != 0) {
        j += 1
        f(elems(i).asInstanceOf[A])
      }
      i += 1
    }
  }

  /** Applies a function to all keys of this map. */
  def foreachKey[A](f: K => A) { foreachElement[K,A](_keys, f) }

  /** Applies a function to all values of this map. */
  def foreachValue[A](f: V => A) { foreachElement[V,A](_values, f) }

  /** Creates a new `AnyRefMap` with different values.
   *  Unlike `mapValues`, this method generates a new
   *  collection immediately.
   */
  def mapValuesNow[V1](f: V => V1): AnyRefMap[K, V1] = {
    val arm = new AnyRefMap[K,V1](AnyRefMap.exceptionDefault,  1,  false)
    val hz = java.util.Arrays.copyOf(_hashes, _hashes.length)
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = new Array[AnyRef](_values.length)
    var i,j = 0
    while (i < _hashes.length & j < _size) {
      val h = _hashes(i)
      if (h+h != 0) {
        j += 1
        vz(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
      }
      i += 1
    }
    arm.initializeTo(mask, _size, _vacant, hz, kz, vz)
    arm
  }

  /** Applies a transformation function to all values stored in this map.
   *  Note: the default, if any,  is not transformed.
   */
  def transformValues(f: V => V): this.type = {
    var i,j = 0
    while (i < _hashes.length & j < _size) {
      val h = _hashes(i)
      if (h+h != 0) {
        j += 1
        _values(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
      }
      i += 1
    }
    this
  }

}

object AnyRefMap {
  private final val IndexMask  = 0x3FFFFFFF
  private final val MissingBit = 0x80000000
  private final val VacantBit  = 0x40000000
  private final val MissVacant = 0xC0000000

  private val exceptionDefault = (k: Any) => throw new NoSuchElementException(if (k == null) "(null)" else k.toString)

  implicit def canBuildFrom[K <: AnyRef, V, J <: AnyRef, U]: CanBuildFrom[AnyRefMap[K,V], (J, U), AnyRefMap[J,U]] =
    new CanBuildFrom[AnyRefMap[K,V], (J, U), AnyRefMap[J,U]] {
      def apply(from: AnyRefMap[K,V]): AnyRefMapBuilder[J, U] = apply()
      def apply(): AnyRefMapBuilder[J, U] = new AnyRefMapBuilder[J, U]
    }

  final class AnyRefMapBuilder[K <: AnyRef, V] extends Builder[(K, V), AnyRefMap[K, V]] {
    private[collection] var elems: AnyRefMap[K, V] = new AnyRefMap[K, V]
    def +=(entry: (K, V)): this.type = {
      elems += entry
      this
    }
    def clear() { elems = new AnyRefMap[K, V] }
    def result(): AnyRefMap[K, V] = elems
  }

  /** Creates a new `AnyRefMap` with zero or more key/value pairs. */
  def apply[K <: AnyRef, V](elems: (K, V)*): AnyRefMap[K, V] = {
    val sz = if (elems.hasDefiniteSize) elems.size else 4
    val arm = new AnyRefMap[K, V](sz * 2)
    elems.foreach{ case (k,v) => arm(k) = v }
    if (arm.size < (sz>>3)) arm.repack()
    arm
  }

  /** Creates a new empty `AnyRefMap`. */
  def empty[K <: AnyRef, V]: AnyRefMap[K, V] = new AnyRefMap[K, V]

  /** Creates a new empty `AnyRefMap` with the supplied default */
  def withDefault[K <: AnyRef, V](default: K => V): AnyRefMap[K, V] = new AnyRefMap[K, V](default)

  /** Creates a new `AnyRefMap` from arrays of keys and values.
   *  Equivalent to but more efficient than `AnyRefMap((keys zip values): _*)`.
   */
  def fromZip[K <: AnyRef, V](keys: Array[K], values: Array[V]): AnyRefMap[K, V] = {
    val sz = math.min(keys.length, values.length)
    val arm = new AnyRefMap[K, V](sz * 2)
    var i = 0
    while (i < sz) { arm(keys(i)) = values(i); i += 1 }
    if (arm.size < (sz>>3)) arm.repack()
    arm
  }

  /** Creates a new `AnyRefMap` from keys and values.
   *  Equivalent to but more efficient than `AnyRefMap((keys zip values): _*)`.
   */
  def fromZip[K <: AnyRef, V](keys: Iterable[K], values: Iterable[V]): AnyRefMap[K, V] = {
    val sz = math.min(keys.size, values.size)
    val arm = new AnyRefMap[K, V](sz * 2)
    val ki = keys.iterator
    val vi = values.iterator
    while (ki.hasNext && vi.hasNext) arm(ki.next) = vi.next
    if (arm.size < (sz >> 3)) arm.repack()
    arm
  }
}
