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

package scala
package collection
package mutable

import scala.annotation.meta.companionClass
import scala.annotation.nowarn
import scala.collection.generic.DefaultSerializationProxy
import scala.language.implicitConversions

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
@(deprecated @companionClass)("Use `scala.collection.mutable.HashMap` instead for better performance.", since = "2.13.16")
class AnyRefMap[K <: AnyRef, V] private[collection] (defaultEntry: K => V, initialBufferSize: Int, initBlank: Boolean)
  extends AbstractMap[K, V]
    with MapOps[K, V, Map, AnyRefMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, AnyRefMap[K, V]]
    with Serializable {

  import AnyRefMap._
  def this() = this(AnyRefMap.exceptionDefault, 16, initBlank = true)

  /** Creates a new `AnyRefMap` that returns default values according to a supplied key-value mapping. */
  def this(defaultEntry: K => V) = this(defaultEntry, 16, initBlank = true)

  /** Creates a new `AnyRefMap` with an initial buffer of specified size.
   *
   *  An `AnyRefMap` can typically contain half as many elements as its buffer size
   *  before it requires resizing.
   */
  def this(initialBufferSize: Int) = this(AnyRefMap.exceptionDefault, initialBufferSize, initBlank = true)

  /** Creates a new `AnyRefMap` with specified default values and initial buffer size. */
  def this(defaultEntry: K => V, initialBufferSize: Int) = this(defaultEntry, initialBufferSize, initBlank = true)

  private[this] var mask = 0
  private[this] var _size = 0
  private[this] var _vacant = 0
  private[this] var _hashes: Array[Int] = null
  private[this] var _keys: Array[AnyRef] = null
  private[this] var _values: Array[AnyRef] = null

  if (initBlank) defaultInitialize(initialBufferSize)

  private[this] def defaultInitialize(n: Int): Unit = {
    mask =
      if (n<0) 0x7
      else (((1 << (32 - java.lang.Integer.numberOfLeadingZeros(n-1))) - 1) & 0x3FFFFFFF) | 0x7
    _hashes = new Array[Int](mask+1)
    _keys = new Array[AnyRef](mask+1)
    _values = new Array[AnyRef](mask+1)
  }

  private[collection] def initializeTo(
    m: Int, sz: Int, vc: Int, hz: Array[Int], kz: Array[AnyRef], vz: Array[AnyRef]
  ): Unit = {
    mask = m; _size = sz; _vacant = vc; _hashes = hz; _keys = kz; _values = vz
  }

  override protected def fromSpecific(coll: scala.collection.IterableOnce[(K, V)]): AnyRefMap[K,V] = {
    var sz = coll.knownSize
    if(sz < 0) sz = 4
    val arm = new AnyRefMap[K, V](sz * 2)
    coll.iterator.foreach{ case (k,v) => arm(k) = v }
    if (arm.size < (sz>>3)) arm.repack()
    arm
  }
  override protected def newSpecificBuilder: Builder[(K, V), AnyRefMap[K,V]] = new AnyRefMapBuilder

  override def size: Int = _size
  override def knownSize: Int = size
  override def isEmpty: Boolean = _size == 0
  override def empty: AnyRefMap[K,V] = new AnyRefMap(defaultEntry)

  private def imbalanced: Boolean =
    (_size + _vacant) > 0.5*mask || _vacant > _size

  private def hashOf(key: K): Int = {
    // Note: this method must not return 0 or Int.MinValue, as these indicate no element
    if (key eq null) 0x41081989
    else {
      val h = key.hashCode
      // Part of the MurmurHash3 32 bit finalizer
      val i = (h ^ (h >>> 16)) * 0x85EBCA6B
      val j = (i ^ (i >>> 13)) & 0x7FFFFFFF
      if (j==0) 0x41081989 else j
    }
  }

  private def seekEntry(h: Int, k: AnyRef): Int = {
    var e = h & mask
    var x = 0
    var g = 0
    val hashes = _hashes
    val keys = _keys
    while ({ g = hashes(e); g != 0}) {
      if (g == h && { val q = keys(e); (q eq k) || ((q ne null) && (q equals k)) }) return e
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    e | MissingBit
  }

  @`inline` private def seekEntryOrOpen(h: Int, k: AnyRef): Int = {
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
      val value = {
        val ohs = _hashes
        val j = i & IndexMask
        val oh = ohs(j)
        val ans = defaultValue
        // Evaluating `defaultValue` may change the map
        //   - repack: the array is different
        //   - element added at `j`: since `i < 0`, the key was missing and `oh` is either 0 or MinValue.
        //     If `defaultValue` added an element at `j` then `_hashes(j)` must be different now.
        //     (`hashOf` never returns 0 or MinValue.)
        if (ohs.ne(_hashes) || oh != _hashes(j)) {
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
  override def default(key: K): V = defaultEntry(key)

  private def repack(newMask: Int): Unit = {
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
  def repack(): Unit = {
    var m = mask
    if (_size + _vacant >= 0.5*mask && !(_vacant > 0.2*mask)) m = ((m << 1) + 1) & IndexMask
    while (m > 8 && 8*_size < m) m = m >>> 1
    repack(m)
  }

  override def put(key: K, value: V): Option[V] = {
    val h = hashOf(key)
    val i = seekEntryOrOpen(h, key)
    if (i < 0) {
      val j = i & IndexMask
      _hashes(j) = h
      _keys(j) = key
      _values(j) = value.asInstanceOf[AnyRef]
      _size += 1
      if ((i & VacantBit) != 0) _vacant -= 1
      else if (imbalanced) repack()
      None
    }
    else {
      val ans = Some(_values(i).asInstanceOf[V])
      _hashes(i) = h
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
    val i = seekEntryOrOpen(h, key)
    if (i < 0) {
      val j = i & IndexMask
      _hashes(j) = h
      _keys(j) = key
      _values(j) = value.asInstanceOf[AnyRef]
      _size += 1
      if ((i & VacantBit) != 0) _vacant -= 1
      else if (imbalanced) repack()
    }
    else {
      _hashes(i) = h
      _values(i) = value.asInstanceOf[AnyRef]
    }
  }

  /** Adds a new key/value pair to this map and returns the map. */
  @deprecated("Use `addOne` or `update` instead; infix operations with an operand of multiple args will be deprecated", "2.13.3")
  def +=(key: K, value: V): this.type = { update(key, value); this }

  /** Adds a new key/value pair to this map and returns the map. */
  @inline final def addOne(key: K, value: V): this.type = { update(key, value); this }

  @inline override final def addOne(kv: (K, V)): this.type = { update(kv._1, kv._2); this }

  def subtractOne(key: K): this.type = {
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

  def iterator: Iterator[(K, V)] = new AnyRefMapIterator[(K, V)] {
    protected def nextResult(k: K, v: V) = (k, v)
  }
  override def keysIterator: Iterator[K] = new AnyRefMapIterator[K] {
    protected def nextResult(k: K, v: V) = k
  }
  override def valuesIterator: Iterator[V] = new AnyRefMapIterator[V] {
    protected def nextResult(k: K, v: V) = v
  }

  private abstract class AnyRefMapIterator[A] extends AbstractIterator[A] {
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

    def next(): A = {
      if (hasNext) {
        val ans = nextResult(kz(index).asInstanceOf[K], vz(index).asInstanceOf[V])
        index += 1
        ans
      }
      else throw new NoSuchElementException("next")
    }

    protected def nextResult(k: K, v: V): A
  }


  override def foreach[U](f: ((K,V)) => U): Unit = {
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

  override def foreachEntry[U](f: (K,V) => U): Unit = {
    var i = 0
    var e = _size
    while (e > 0) {
      while(i < _hashes.length && { val h = _hashes(i); h+h == 0 && i < _hashes.length}) i += 1
      if (i < _hashes.length) {
        f(_keys(i).asInstanceOf[K], _values(i).asInstanceOf[V])
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
    val arm = new AnyRefMap[K, V](defaultEntry, 1, initBlank = false)
    arm.initializeTo(mask, _size, _vacant, hz, kz,  vz)
    arm
  }

  @deprecated("Consider requiring an immutable Map or fall back to Map.concat", "2.13.0")
  override def + [V1 >: V](kv: (K, V1)): AnyRefMap[K, V1] = AnyRefMap.from(new View.Appended(this, kv))

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): AnyRefMap[K, V1] = {
    val m = this + elem1 + elem2
    if(elems.isEmpty) m else m.concat(elems)
  }

  override def concat[V2 >: V](xs: scala.collection.IterableOnce[(K, V2)]): AnyRefMap[K, V2] = {
    val arm = clone().asInstanceOf[AnyRefMap[K, V2]]
    xs.iterator.foreach(kv => arm += kv)
    arm
  }

  override def ++[V2 >: V](xs: scala.collection.IterableOnce[(K, V2)]): AnyRefMap[K, V2] = concat(xs)

  @deprecated("Use m.clone().addOne(k,v) instead of m.updated(k, v)", "2.13.0")
  override def updated[V1 >: V](key: K, value: V1): AnyRefMap[K, V1] =
    clone().asInstanceOf[AnyRefMap[K, V1]].addOne(key, value)

  private[this] def foreachElement[A,B](elems: Array[AnyRef], f: A => B): Unit = {
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
  def foreachKey[A](f: K => A): Unit = foreachElement[K,A](_keys, f)

  /** Applies a function to all values of this map. */
  def foreachValue[A](f: V => A): Unit = foreachElement[V,A](_values, f)

  /** Creates a new `AnyRefMap` with different values.
   *  Unlike `mapValues`, this method generates a new
   *  collection immediately.
   */
  def mapValuesNow[V1](f: V => V1): AnyRefMap[K, V1] = {
    val arm = new AnyRefMap[K,V1](AnyRefMap.exceptionDefault,  1,  initBlank = false)
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
  @deprecated("Use transformValuesInPlace instead of transformValues", "2.13.0")
  @`inline` final def transformValues(f: V => V): this.type = transformValuesInPlace(f)

  /** Applies a transformation function to all values stored in this map.
   *  Note: the default, if any,  is not transformed.
   */
  def transformValuesInPlace(f: V => V): this.type = {
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

  // The implicit dummy parameter is necessary to distinguish these methods from the base methods they overload (not override).
  // Previously, in Scala 2, f took `K with AnyRef` scala/bug#11035
  /**
   * An overload of `map` which produces an `AnyRefMap`.
   *
   * @param f the mapping function must produce a key-value pair where the key is an `AnyRef`
   * @param dummy an implicit placeholder for purposes of distinguishing the (erased) signature of this method
   */
  def map[K2 <: AnyRef, V2](f: ((K, V)) => (K2, V2))(implicit dummy: DummyImplicit): AnyRefMap[K2, V2] =
    AnyRefMap.from(new View.Map(this, f))
  /**
   * An overload of `flatMap` which produces an `AnyRefMap`.
   *
   * @param f the mapping function must produce key-value pairs where the key is an `AnyRef`
   * @param dummy an implicit placeholder for purposes of distinguishing the (erased) signature of this method
   */
  def flatMap[K2 <: AnyRef, V2](f: ((K, V)) => IterableOnce[(K2, V2)])(implicit dummy: DummyImplicit): AnyRefMap[K2, V2] =
    AnyRefMap.from(new View.FlatMap(this, f))
  /**
   * An overload of `collect` which produces an `AnyRefMap`.
   *
   * @param pf the mapping function must produce a key-value pair where the key is an `AnyRef`
   * @param dummy an implicit placeholder for purposes of distinguishing the (erased) signature of this method
   */
  def collect[K2 <: AnyRef, V2](pf: PartialFunction[(K, V), (K2, V2)])(implicit dummy: DummyImplicit): AnyRefMap[K2, V2] =
    strictOptimizedCollect(AnyRefMap.newBuilder[K2, V2], pf)

  override def clear(): Unit = {
    import java.util.Arrays.fill
    fill(_keys, null)
    fill(_values, null)
    fill(_hashes, 0)
    _size = 0
    _vacant = 0
  }

  protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(AnyRefMap.toFactory[K, V](AnyRefMap), this)

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix = "AnyRefMap"
}

@deprecated("Use `scala.collection.mutable.HashMap` instead for better performance.", since = "2.13.16")
object AnyRefMap {
  private final val IndexMask  = 0x3FFFFFFF
  private final val MissingBit = 0x80000000
  private final val VacantBit  = 0x40000000
  private final val MissVacant = 0xC0000000

  private class ExceptionDefault extends (Any => Nothing) with Serializable {
    def apply(k: Any): Nothing = throw new NoSuchElementException(if (k == null) "(null)" else k.toString)
  }
  private val exceptionDefault = new ExceptionDefault

  /** A builder for instances of `AnyRefMap`.
   *
   *  This builder can be reused to create multiple instances.
   */
  final class AnyRefMapBuilder[K <: AnyRef, V] extends ReusableBuilder[(K, V), AnyRefMap[K, V]] {
    private[collection] var elems: AnyRefMap[K, V] = new AnyRefMap[K, V]
    def addOne(entry: (K, V)): this.type = {
      elems += entry
      this
    }
    def clear(): Unit = elems = new AnyRefMap[K, V]
    def result(): AnyRefMap[K, V] = elems
    override def knownSize: Int = elems.knownSize
  }

  /** Creates a new `AnyRefMap` with zero or more key/value pairs. */
  def apply[K <: AnyRef, V](elems: (K, V)*): AnyRefMap[K, V] = buildFromIterableOnce(elems)

  def newBuilder[K <: AnyRef, V]: ReusableBuilder[(K, V), AnyRefMap[K, V]] = new AnyRefMapBuilder[K, V]

  private def buildFromIterableOnce[K <: AnyRef, V](elems: IterableOnce[(K, V)]): AnyRefMap[K, V] = {
    var sz = elems.knownSize
    if(sz < 0) sz = 4
    val arm = new AnyRefMap[K, V](sz * 2)
    elems.iterator.foreach{ case (k,v) => arm(k) = v }
    if (arm.size < (sz>>3)) arm.repack()
    arm
  }

  /** Creates a new empty `AnyRefMap`. */
  def empty[K <: AnyRef, V]: AnyRefMap[K, V] = new AnyRefMap[K, V]

  /** Creates a new empty `AnyRefMap` with the supplied default */
  def withDefault[K <: AnyRef, V](default: K => V): AnyRefMap[K, V] = new AnyRefMap[K, V](default)

  /** Creates a new `AnyRefMap` from an existing source collection. A source collection
    * which is already an `AnyRefMap` gets cloned.
    *
    * @param source Source collection
    * @tparam K the type of the keys
    * @tparam V the type of the values
    * @return a new `AnyRefMap` with the elements of `source`
    */
  def from[K <: AnyRef, V](source: IterableOnce[(K, V)]): AnyRefMap[K, V] = source match {
    case source: AnyRefMap[_, _] => source.clone().asInstanceOf[AnyRefMap[K, V]]
    case _ => buildFromIterableOnce(source)
  }

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
    while (ki.hasNext && vi.hasNext) arm(ki.next()) = vi.next()
    if (arm.size < (sz >> 3)) arm.repack()
    arm
  }

  implicit def toFactory[K <: AnyRef, V](dummy: AnyRefMap.type): Factory[(K, V), AnyRefMap[K, V]] = ToFactory.asInstanceOf[Factory[(K, V), AnyRefMap[K, V]]]

  @SerialVersionUID(3L)
  private[this] object ToFactory extends Factory[(AnyRef, AnyRef), AnyRefMap[AnyRef, AnyRef]] with Serializable {
    def fromSpecific(it: IterableOnce[(AnyRef, AnyRef)]): AnyRefMap[AnyRef, AnyRef] = AnyRefMap.from[AnyRef, AnyRef](it)
    def newBuilder: Builder[(AnyRef, AnyRef), AnyRefMap[AnyRef, AnyRef]] = AnyRefMap.newBuilder[AnyRef, AnyRef]
  }

  implicit def toBuildFrom[K <: AnyRef, V](factory: AnyRefMap.type): BuildFrom[Any, (K, V), AnyRefMap[K, V]] = ToBuildFrom.asInstanceOf[BuildFrom[Any, (K, V), AnyRefMap[K, V]]]
  private[this] object ToBuildFrom extends BuildFrom[Any, (AnyRef, AnyRef), AnyRefMap[AnyRef, AnyRef]] {
    def fromSpecific(from: Any)(it: IterableOnce[(AnyRef, AnyRef)]): AnyRefMap[AnyRef, AnyRef] = AnyRefMap.from(it)
    def newBuilder(from: Any): ReusableBuilder[(AnyRef, AnyRef), AnyRefMap[AnyRef, AnyRef]] = AnyRefMap.newBuilder[AnyRef, AnyRef]
  }

  implicit def iterableFactory[K <: AnyRef, V]: Factory[(K, V), AnyRefMap[K, V]] = toFactory[K, V](this)
  implicit def buildFromAnyRefMap[K <: AnyRef, V]: BuildFrom[AnyRefMap[_, _], (K, V), AnyRefMap[K, V]] = toBuildFrom(this)
}
