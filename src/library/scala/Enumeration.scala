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

import scala.collection.{SpecificIterableFactory, StrictOptimizedIterableOps, View, immutable, mutable}
import java.lang.reflect.{Field => JField, Method => JMethod}

import scala.annotation.{implicitNotFound, tailrec}
import scala.reflect.NameTransformer._
import scala.util.matching.Regex

/** Defines a finite set of values specific to the enumeration. Typically
 *  these values enumerate all possible forms something can take and provide
 *  a lightweight alternative to case classes.
 *
 *  Each call to a `Value` method adds a new unique value to the enumeration.
 *  To be accessible, these values are usually defined as `val` members of
 *  the enumeration.
 *
 *  All values in an enumeration share a common, unique type defined as the
 *  `Value` type member of the enumeration (`Value` selected on the stable
 *  identifier path of the enumeration instance).
 *
 *  Values SHOULD NOT be added to an enumeration after its construction;
 *  doing so makes the enumeration thread-unsafe. If values are added to an
 *  enumeration from multiple threads (in a non-synchronized fashion) after
 *  construction, the behavior of the enumeration is undefined.
 *
 * @example {{{
 * // Define a new enumeration with a type alias and work with the full set of enumerated values
 * object WeekDay extends Enumeration {
 *   type WeekDay = Value
 *   val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
 * }
 * import WeekDay._
 *
 * def isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)
 *
 * WeekDay.values filter isWorkingDay foreach println
 * // output:
 * // Mon
 * // Tue
 * // Wed
 * // Thu
 * // Fri
 * }}}
 *
 * @example {{{
 * // Example of adding attributes to an enumeration by extending the Enumeration.Val class
 * object Planet extends Enumeration {
 *   protected case class PlanetVal(mass: Double, radius: Double) extends super.Val {
 *     def surfaceGravity: Double = Planet.G * mass / (radius * radius)
 *     def surfaceWeight(otherMass: Double): Double = otherMass * surfaceGravity
 *   }
 *   import scala.language.implicitConversions
 *   implicit def valueToPlanetVal(x: Value): PlanetVal = x.asInstanceOf[PlanetVal]
 *
 *   val G: Double = 6.67300E-11
 *   val Mercury = PlanetVal(3.303e+23, 2.4397e6)
 *   val Venus   = PlanetVal(4.869e+24, 6.0518e6)
 *   val Earth   = PlanetVal(5.976e+24, 6.37814e6)
 *   val Mars    = PlanetVal(6.421e+23, 3.3972e6)
 *   val Jupiter = PlanetVal(1.9e+27, 7.1492e7)
 *   val Saturn  = PlanetVal(5.688e+26, 6.0268e7)
 *   val Uranus  = PlanetVal(8.686e+25, 2.5559e7)
 *   val Neptune = PlanetVal(1.024e+26, 2.4746e7)
 * }
 *
 * println(Planet.values.filter(_.radius > 7.0e6))
 * // output:
 * // Planet.ValueSet(Jupiter, Saturn, Uranus, Neptune)
 * }}}
 *
 *  @param initial The initial value from which to count the integers that
 *                 identifies values at run-time.
 */
@SerialVersionUID(8476000850333817230L)
abstract class Enumeration (initial: Int) extends Serializable {
  thisenum =>

  def this() = this(0)

  /* Note that `readResolve` cannot be private, since otherwise
     the JVM does not invoke it when deserializing subclasses. */
  protected def readResolve(): AnyRef = thisenum.getClass.getField(MODULE_INSTANCE_NAME).get(null)

  /** The name of this enumeration.
   */
  override def toString: String =
    ((getClass.getName stripSuffix MODULE_SUFFIX_STRING split '.').last split
       Regex.quote(NAME_JOIN_STRING)).last

  /** The mapping from the integer used to identify values to the actual
    * values. */
  private val vmap: mutable.Map[Int, Value] = new mutable.HashMap

  /** The cache listing all values of this enumeration. */
  @transient private var vset: ValueSet = null
  @transient @volatile private var vsetDefined = false

  /** The mapping from the integer used to identify values to their
    * names. */
  private[this] val nmap: mutable.Map[Int, String] = new mutable.HashMap

  /** The values of this enumeration as a set.
   */
  def values: ValueSet = {
    if (!vsetDefined) {
      vset = (ValueSet.newBuilder ++= vmap.values).result()
      vsetDefined = true
    }
    vset
  }

  /** The integer to use to identify the next created value. */
  protected var nextId: Int = initial

  /** The string to use to name the next created value. */
  protected var nextName: Iterator[String] = _

  private def nextNameOrNull =
    if (nextName != null && nextName.hasNext) nextName.next() else null

  /** The highest integer amongst those used to identify values in this
    * enumeration. */
  private[this] var topId = initial

  /** The lowest integer amongst those used to identify values in this
    * enumeration, but no higher than 0. */
  private[this] var bottomId = if(initial < 0) initial else 0

  /** The one higher than the highest integer amongst those used to identify
    *  values in this enumeration. */
  final def maxId = topId

  /** The value of this enumeration with given id `x`
   */
  final def apply(x: Int): Value = vmap(x)

  /** Return a `Value` from this `Enumeration` whose name matches
   *  the argument `s`.  The names are determined automatically via reflection.
   *
   * @param  s an `Enumeration` name
   * @return   the `Value` of this `Enumeration` if its name matches `s`
   * @throws   NoSuchElementException if no `Value` with a matching
   *           name is in this `Enumeration`
   */
  final def withName(s: String): Value = values.byName.getOrElse(s,
    throw new NoSuchElementException(s"No value found for '$s'"))

  /** Creates a fresh value, part of this enumeration. */
  protected final def Value: Value = Value(nextId)

  /** Creates a fresh value, part of this enumeration, identified by the
   *  integer `i`.
   *
   *  @param i An integer that identifies this value at run-time. It must be
   *           unique amongst all values of the enumeration.
   *  @return  Fresh value identified by `i`.
   */
  protected final def Value(i: Int): Value = Value(i, nextNameOrNull)

  /** Creates a fresh value, part of this enumeration, called `name`.
   *
   *  @param name A human-readable name for that value.
   *  @return  Fresh value called `name`.
   */
  protected final def Value(name: String): Value = Value(nextId, name)

  /** Creates a fresh value, part of this enumeration, called `name`
   *  and identified by the integer `i`.
   *
   * @param i    An integer that identifies this value at run-time. It must be
   *             unique amongst all values of the enumeration.
   * @param name A human-readable name for that value.
   * @return     Fresh value with the provided identifier `i` and name `name`.
   */
  protected final def Value(i: Int, name: String): Value = new Val(i, name)

  private def populateNameMap(): Unit = {
    @tailrec def getFields(clazz: Class[_], acc: Array[JField]): Array[JField] = {
      if (clazz == null)
        acc
      else
        getFields(clazz.getSuperclass, if (clazz.getDeclaredFields.isEmpty) acc else acc ++ clazz.getDeclaredFields)
    }
    val fields = getFields(getClass.getSuperclass, getClass.getDeclaredFields)
    def isValDef(m: JMethod): Boolean = fields exists (fd => fd.getName == m.getName && fd.getType == m.getReturnType)

    // The list of possible Value methods: 0-args which return a conforming type
    val methods: Array[JMethod] = getClass.getMethods filter (m => m.getParameterTypes.isEmpty &&
                                                                   classOf[Value].isAssignableFrom(m.getReturnType) &&
                                                                   m.getDeclaringClass != classOf[Enumeration] &&
                                                                   isValDef(m))
    methods foreach { m =>
      val name = m.getName
      // invoke method to obtain actual `Value` instance
      val value = m.invoke(this).asInstanceOf[Value]
      // verify that outer points to the correct Enumeration: ticket #3616.
      if (value.outerEnum eq thisenum) {
        val id: Int = value.id
        nmap += ((id, name))
      }
    }
  }

  /* Obtains the name for the value with id `i`. If no name is cached
   * in `nmap`, it populates `nmap` using reflection.
   */
  private def nameOf(i: Int): String = synchronized { nmap.getOrElse(i, { populateNameMap() ; nmap(i) }) }

  /** The type of the enumerated values. */
  @SerialVersionUID(7091335633555234129L)
  abstract class Value extends Ordered[Value] with Serializable {
    /** the id and bit location of this enumeration value */
    def id: Int
    /** a marker so we can tell whose values belong to whom come reflective-naming time */
    private[Enumeration] val outerEnum = thisenum

    override def compare(that: Value): Int =
      if (this.id < that.id) -1
      else if (this.id == that.id) 0
      else 1
    override def equals(other: Any): Boolean = other match {
      case that: Enumeration#Value  => (outerEnum eq that.outerEnum) && (id == that.id)
      case _                        => false
    }
    override def hashCode: Int = id.##

    /** Create a ValueSet which contains this value and another one */
    def + (v: Value): ValueSet = ValueSet(this, v)
  }

  /** A class implementing the [[scala.Enumeration.Value]] type. This class
   *  can be overridden to change the enumeration's naming and integer
   *  identification behaviour.
   */
  @SerialVersionUID(0 - 3501153230598116017L)
  protected class Val(i: Int, name: String) extends Value with Serializable {
    def this(i: Int)       = this(i, nextNameOrNull)
    def this(name: String) = this(nextId, name)
    def this()             = this(nextId)

    assert(!vmap.isDefinedAt(i), "Duplicate id: " + i)
    vmap(i) = this
    vsetDefined = false
    nextId = i + 1
    if (nextId > topId) topId = nextId
    if (i < bottomId) bottomId = i
    def id: Int = i
    override def toString(): String =
      if (name != null) name
      else try thisenum.nameOf(i)
      catch { case _: NoSuchElementException => "<Invalid enum: no field for #" + i + ">" }

    protected def readResolve(): AnyRef = {
      val enumeration = thisenum.readResolve().asInstanceOf[Enumeration]
      if (enumeration.vmap == null) this
      else enumeration.vmap(i)
    }
  }

  /** An ordering by id for values of this set */
  implicit object ValueOrdering extends Ordering[Value] {
    def compare(x: Value, y: Value): Int = x compare y
  }

  /** A class for sets of values.
   *  Iterating through this set will yield values in increasing order of their ids.
   *
   *  @param nnIds The set of ids of values (adjusted so that the lowest value does
   *    not fall below zero), organized as a `BitSet`.
   *  @define Coll `collection.immutable.SortedSet`
   */
  @SerialVersionUID(7229671200427364242L)
  class ValueSet private[ValueSet] (private[this] var nnIds: immutable.BitSet)
    extends immutable.AbstractSet[Value]
      with immutable.SortedSet[Value]
      with immutable.SortedSetOps[Value, immutable.SortedSet, ValueSet]
      with StrictOptimizedIterableOps[Value, immutable.Set, ValueSet]
      with Serializable {

    implicit def ordering: Ordering[Value] = ValueOrdering
    def rangeImpl(from: Option[Value], until: Option[Value]): ValueSet =
      new ValueSet(nnIds.rangeImpl(from.map(_.id - bottomId), until.map(_.id - bottomId)))

    override def empty: ValueSet = ValueSet.empty
    override def knownSize: Int = nnIds.size
    override def isEmpty: Boolean = nnIds.isEmpty
    def contains(v: Value): Boolean = nnIds contains (v.id - bottomId)
    def incl (value: Value): ValueSet = new ValueSet(nnIds + (value.id - bottomId))
    def excl (value: Value): ValueSet = new ValueSet(nnIds - (value.id - bottomId))
    def iterator: Iterator[Value] = nnIds.iterator map (id => thisenum.apply(bottomId + id))
    override def iteratorFrom(start: Value): Iterator[Value] = nnIds iteratorFrom start.id  map (id => thisenum.apply(bottomId + id))
    override def className: String = s"$thisenum.ValueSet"
    /** Creates a bit mask for the zero-adjusted ids in this set as a
     *  new array of longs */
    def toBitMask: Array[Long] = nnIds.toBitMask

    override protected def fromSpecific(coll: IterableOnce[Value]): ValueSet = ValueSet.fromSpecific(coll)
    override protected def newSpecificBuilder = ValueSet.newBuilder

    def map(f: Value => Value): ValueSet = fromSpecific(new View.Map(this, f))
    def flatMap(f: Value => IterableOnce[Value]): ValueSet = fromSpecific(new View.FlatMap(this, f))

    // necessary for disambiguation:
    override def map[B](f: Value => B)(implicit @implicitNotFound(ValueSet.ordMsg) ev: Ordering[B]): immutable.SortedSet[B] =
      super[SortedSet].map[B](f)
    override def flatMap[B](f: Value => IterableOnce[B])(implicit @implicitNotFound(ValueSet.ordMsg) ev: Ordering[B]): immutable.SortedSet[B] =
      super[SortedSet].flatMap[B](f)
    override def zip[B](that: IterableOnce[B])(implicit @implicitNotFound(ValueSet.zipOrdMsg) ev: Ordering[(Value, B)]): immutable.SortedSet[(Value, B)] =
      super[SortedSet].zip[B](that)
    override def collect[B](pf: PartialFunction[Value, B])(implicit @implicitNotFound(ValueSet.ordMsg) ev: Ordering[B]): immutable.SortedSet[B] =
      super[SortedSet].collect[B](pf)

    @transient private[Enumeration] lazy val byName: Map[String, Value] = iterator.map( v => v.toString -> v).toMap
  }

  /** A factory object for value sets */
  @SerialVersionUID(3L)
  object ValueSet extends SpecificIterableFactory[Value, ValueSet] {
    private final val ordMsg = "No implicit Ordering[${B}] found to build a SortedSet[${B}]. You may want to upcast to a Set[Value] first by calling `unsorted`."
    private final val zipOrdMsg = "No implicit Ordering[${B}] found to build a SortedSet[(Value, ${B})]. You may want to upcast to a Set[Value] first by calling `unsorted`."

    /** The empty value set */
    val empty: ValueSet = new ValueSet(immutable.BitSet.empty)
    /** A value set containing all the values for the zero-adjusted ids
     *  corresponding to the bits in an array */
    def fromBitMask(elems: Array[Long]): ValueSet = new ValueSet(immutable.BitSet.fromBitMask(elems))
    /** A builder object for value sets */
    def newBuilder: mutable.Builder[Value, ValueSet] = new mutable.Builder[Value, ValueSet] {
      private[this] val b = new mutable.BitSet
      def addOne (x: Value) = { b += (x.id - bottomId); this }
      def clear() = b.clear()
      def result() = new ValueSet(b.toImmutable)
    }
    def fromSpecific(it: IterableOnce[Value]): ValueSet =
      newBuilder.addAll(it).result()
  }
}
