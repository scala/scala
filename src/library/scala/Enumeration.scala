/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import scala.collection.mutable.{Map, HashMap}
import scala.collection.immutable.{Set, BitSet}
import scala.collection.generic.{Builder, BuilderFactory, AddingBuilder, SetTemplate}

/** <p>
 *    Defines a finite set of values specific to the enumeration. Typically
 *    these values enumerate all possible forms something can take and provide a
 *    lightweight alternative to case classes.
 *  </p>
 *  <p>
 *    Each call to a <code>Value</code> method adds a new unique value to the
 *    enumeration. To be accessible, these values are usually defined as
 *    <code>val</code> members of the evaluation.
 *  </p>
 *  <p>
 *    All values in an enumeration share a common, unique type defined as the
 *    <code>Value</code> type member of the enumeration (<code>Value</code>
 *    selected on the stable identifier path of the enumeration instance).
 *  </p>
 *  <p>
 *    <b>Example use</b>
 *  </p><pre>
 *  <b>object</b> Main <b>extends</b> Application {
 *
 *    <b>object</b> WeekDay <b>extends</b> Enumeration {
 *      <b>type</b> WeekDay</b> = Value
 *      <b>val</b> Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
 *    }
 *    <b>import</b> WeekDay._
 *
 *    <b>def</b> isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)
 *
 *    WeekDay.elements filter isWorkingDay foreach println
 *  }</pre>
 *
 *  @param initial The initial value from which to count the integers that
 *                 identifies values at run-time.
 *  @param names   The sequence of names to give to this enumeration's values.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/02/2004
 */
@serializable
@SerialVersionUID(8476000850333817230L)
abstract class Enumeration(initial: Int, names: String*) {

  def this() = this(0, null)
  def this(names: String*) = this(0, names: _*)

  /** The name of this enumeration.
   */
  override def toString = {
    val cname = this.getClass().getName()
    if (cname endsWith "$")
      cname.substring(0, cname.length() - 1)
    else if (cname endsWith "$class")
      cname.substring(0, cname.length() - 6)
    else
      cname
  }

  /** The mapping from the integer used to identifying values to the actual
    * values. */
  private val vmap: Map[Int, Value] = new HashMap

  /** The cache listing all values of this enumeration. */
  @transient private var vset: ValueSet = null
  @transient private var vsetDefined = false

  /** The values of this enumeration as a set.
   */
  def values: ValueSet = {
    if (!vsetDefined) {
      vset = new ValueSet(BitSet.empty ++ (vmap.values map (_.id)))
      vsetDefined = true
    }
    vset
  }

  /** The integer to use to identify the next created value. */
  protected var nextId = initial

  /** The string to use to name the next created value. */
  protected var nextName = names.elements

  /** The highest integer amongst those used to identify values in this
    * enumeration. */
  private var topId = initial

  /** The highest integer amongst those used to identify values in this
    * enumeration. */
  final def maxId = topId

  /** The value of this enumeration with given id `x`
   */
  final def withId(x: Int): Value = vmap(x)

  /** Returns a Value from this Enumeration whose name matches
   * the argument <var>s</var>.
   * You must pass a String* set of names to the constructor,
   * or initialize each Enumeration with Value(String),
   * for valueOf to work.
   * @param s an enumeration name
   * @return <tt>Some(Value)</tt> if an enumeration's name matches <var>s</var>,
   * else <tt>None</tt>
   */
  def withName(s: String): Value = values.find(_.toString == s).get

  /** Creates a fresh value, part of this enumeration. */
  protected final def Value: Value = Value(nextId)

  /** Creates a fresh value, part of this enumeration, identified by the integer
   *  <code>i</code>.
   *
   *  @param i An integer that identifies this value at run-time. It must be
   *           unique amongst all values of the enumeration.
   *  @return  ..
   */
  protected final def Value(i: Int): Value =
    Value(i, if (nextName.hasNext) nextName.next else null)

  /** Creates a fresh value, part of this enumeration, called <code>name</code>.
   *
   *  @param name A human-readable name for that value.
   */
  protected final def Value(name: String): Value = Value(nextId, name)

  /** Creates a fresh value, part of this enumeration, called <code>name</code>
   *  and identified by the integer <code>i</code>.
   *
   * @param i    An integer that identifies this value at run-time. It must be
   *             unique amongst all values of the enumeration.
   * @param name A human-readable name for that value.
   * @return     ..
   */
  protected final def Value(i: Int, name: String): Value = new Val(i, name)

  /** The type of the enumerated values. */
  @serializable
  @SerialVersionUID(7091335633555234129L)
  abstract class Value extends Ordered[Value] {
    /** the id and bit location of this enumeration value */
    def id: Int
    override def compare(that: Value): Int = this.id - that.id
    override def equals(other: Any): Boolean =
      other match {
        case that: Value => compare(that) == 0
        case _ => false
      }
    override def hashCode: Int = id.hashCode

    /** this enumeration value as an <code>Int</code> bit mask.
     *  @throws IllegalArgumentException if <code>id</code> is greater than 31
     *  @deprecated
     */
    @deprecated def mask32: Int = {
      if (id >= 32) throw new IllegalArgumentException
      1  << id
    }
    /** this enumeration value as an <code>Long</code> bit mask.
     *  @throws IllegalArgumentException if <code>id</code> is greater than 63
     *  @deprecated
     */
    @deprecated def mask64: Long = {
      if (id >= 64) throw new IllegalArgumentException
      1L << id
    }
  }

  /** A class implementing the <a href="Enumeration.Value.html"
   *  target="contentFrame"><code>Value</code></a> type. This class can be
   *  overriden to change the enumeration's naming and integer identification
   *  behaviour.
   */
  @serializable
  @SerialVersionUID(0 - 3501153230598116017L)
  protected class Val(i: Int, name: String) extends Value {
    def this(i: Int) =
      this(i, if (nextName.hasNext) nextName.next else i.toString())
    def this(name: String) = this(nextId, name)
    def this() =
      this(nextId, if (nextName.hasNext) nextName.next else nextId.toString())
    assert(!vmap.isDefinedAt(i))
    vmap(i) = this
    vsetDefined = false
    nextId = i + 1
    if (nextId > topId) topId = nextId
    def id = i
    override def toString() =
      if (name eq null) Enumeration.this + "(" + i + ")"
      else name
    private def readResolve(): AnyRef =
      if (vmap ne null) vmap(i)
      else this
  }

  /** A class for sets of values
   *  Iterating through this set will yield values in increasing order of their ids.
   *  @param   ids   The set of ids of values, organized as a BitSet.
   */
  class ValueSet private[Enumeration] (val ids: BitSet) extends Set[Value] with SetTemplate[Value, ValueSet] {
    override def empty = ValueSet.empty
    def contains(v: Value) = ids contains (v.id)
    def + (value: Value) = new ValueSet(ids + value.id)
    def - (value: Value) = new ValueSet(ids - value.id)
    def elements = ids.elements map withId
    override def stringPrefix = Enumeration.this + ".ValueSet"
  }

  /** A factory object for value sets */
  object ValueSet {
    /** The empty value set */
    val empty = new ValueSet(BitSet.empty)
    /** A value set consisting of given elements */
    def apply(elems: Value*): ValueSet = {
      var s = empty
      for (elem <- elems) s = s + elem // !!! change to s += elem --> error
      s
    }
    /** A builder object for value sets */
    def newBuilder: Builder[Value, ValueSet] = new AddingBuilder(empty)
    /** The implicit builder for value sets */
    implicit def builderFactory: BuilderFactory[Value, ValueSet, ValueSet] = new BuilderFactory[Value, ValueSet, ValueSet] { def apply(from: ValueSet) = newBuilder }
  }

  /** A set that efficiently stores enumeration values as bits.
   *
   *  @author Sean McDirmid
   *
   *  @ex
   *
   *  <pre>
   *  <b>object</b> flags <b>extends</b> Enumeration {
   *    <b>val</b> Public = Value(5, "public");
   *    <b>val</b> Private = Value(4, "private");
   *    <b>val</b> Protected = Value(6, "protected");
   *    <b>val</b> Final = Value(7, "final");
   *  }
   *
   *  <b>class</b> Entity {
   *    <b>var</b> flags0 : Int = ...;
   *    <b>def</b> flags = flags.Set32(flags0);
   *  }
   *
   *  <b>val</b> e : Entity = ...;
   *
   *  <b>if</b> (e.flags.contains(flags.Private))
   *    e.flags0 = (e.flags | flags.Final).underlying;
   *  </pre>
   *  @deprecated  use ValueSet instead
   */
  @deprecated abstract class SetXX extends collection.immutable.Set[Value] {

    /** either Int or Long */
    type Underlying <: AnyVal
    type TSet <: SetXX

    /** The integer that bit-encodes a set of enumeration values.
     */
    val underlying: Underlying

    /** returns the underlying integer representation of this set as a long. */
    protected def underlyingAsLong: Long

    /** Equivalent to <code>++</code> for bit sets. Returns a set
     *  that has all values in <code>this</code> and <code>set</code>.
     */
    def |(set: TSet): TSet

    /** Equivalent to <code>+</code> for bit sets. Returns a set
     *  that has all values in <code>this</code> with the addition of <code>value</code>.
     */
    def |(value: Value): TSet

    /** Equivalent to <code>**</code> for bit sets.
     *  Returns a bit set that has all values that are both in <code>this</code> and <code>set</code>.
     */
    def &(set: TSet): TSet

    /** Equivalent to <code>-</code> for bit sets.
     *  Returns a bit set that has all values in <code>this</code> except for <code>value</code>.
     */
    def &~(value: Value): TSet
    def -(value: Value): TSet = this &~ value
    def +(value: Value): TSet = this | value
    def ++(set: TSet): TSet = this | set
    def **(set: TSet): TSet = this & set
    override def size = {
      var x = underlyingAsLong
      var sz = 0
      while (x != 0) {
        if ((x & 1) != 0) sz += 1
        x = x >> 1
      }
      sz
    }
    override def stringPrefix = Enumeration.this.name;
    def elements = new Iterator[Value] {
      var bit = 0
      var underlying = underlyingAsLong
      def hasNext = underlying != 0
      private def shift = {
        underlying = underlying >> 1
        bit += 1
      }
      def next = {
        if (underlying == 0) throw new NoSuchElementException
        while ((underlying & 1) == 0) shift
        val ret = vmap(bit)
        shift
        ret
      }
    }
  }

  /** An enumeration bit set that can handle enumeration values with ids up
   *  to 31 in an <code>Int</code>.
   *  @deprecated  use ValueSet instead
   */
  @deprecated class Set32(val underlying: Int) extends SetXX {
    def this() = this(0)
    type Underlying = Int
    type TSet = Set32
    def underlyingAsLong = {
       if (underlying >= 0) underlying.toLong
      else {
        val underlying0 = (~(1 << 31)) & underlying
        assert(underlying0 >= 0)
        underlying0.toLong | (1L << 31)
      }
    }
    def contains(value: Value) = (underlying & value.mask32) != 0
    def |(set: Set32) = new Set32(underlying | set.underlying)
    def |(value: Value) = new Set32(underlying | value.mask32)
    def &~(value: Value) = new Set32(underlying & (~value.mask32))
    def &(set: Set32) = new Set32(underlying & set.underlying)
  }

  /** create an empty 32 bit enumeration set */
  protected def Set32 = new Set32

  /** create a bit enumeration set according to underlying */
  protected def Set32(underlying: Int) = new Set32(underlying)

  /** An enumeration bit set that can handle enumeration values with ids up
   *  to 63 in a <code>Long</code>.
   *  @deprecated  use ValueSet instead
   */
  @deprecated class Set64(val underlying: Long) extends SetXX {
    def this() = this(0)
    type Underlying = Long
    type TSet = Set64
    def underlyingAsLong = underlying
    def contains(value: Value) = (underlying & value.mask64) != 0
    def |(set: Set64) = new Set64(underlying | set.underlying)
    def |(value: Value) = new Set64(underlying | value.mask64)
    def &~(value: Value) = new Set64(underlying & (~value.mask64))
    def &(set: Set64) = new Set64(underlying & set.underlying)
  }

  /** create an empty 64 bit enumeration set */
  protected def Set64 = new Set64

  /** create a bit enumeration set according to underlying */
  protected def Set64(underlying: Long) = new Set64(underlying)

  /** used to reverse engineer bit locations from pre-defined bit masks */
  protected def maskToBit(n: Long) = {
    assert(n != 0)
    var bit = 0
    var m = n
    while ((m & 1) != 1) {
      m = m >> 1
      bit += 1
    }
    assert(m == 1)
    bit
  }

  /** The name of this enumeration.
   *  @deprecated  use toString instead
   */
  @deprecated def name = toString

  /** The value in this enumeration identified by integer <code>x</code>.
   *  @deprecated use withId instead.
   */
  @deprecated final def apply(x: Int): Value = withId(x)

  /** @deprecated use withName instead
   */
  @deprecated def valueOf(s: String) = values.find(_.toString == s)

  /** A new iterator over all values of this enumeration.
   *  @deprecated use values.elements instead
   */
  @deprecated final def elements: Iterator[Value] = values.elements

  /** Apply a function f to all values of this enumeration.
   *  @deprecated use values.foreach instead
   */
  @deprecated def foreach(f: Value => Unit): Unit = elements foreach f

  /** Apply a predicate p to all values of this enumeration and return
    * true, iff the predicate yields true for all values.
   *  @deprecated use values.forall instead
   */
  @deprecated def forall(p: Value => Boolean): Boolean = elements forall p

  /** Apply a predicate p to all values of this enumeration and return
    * true, iff there is at least one value for which p yields true.
    *  @deprecated use values.exists instead
    */
  @deprecated def exists(p: Value => Boolean): Boolean = elements exists p

  /** Returns an iterator resulting from applying the given function f to each
    * value of this enumeration.
    *  @deprecated use values.map instead
    */
  @deprecated def map[B](f: Value => B): Iterator[B] = elements map f

  /** Applies the given function f to each value of this enumeration, then
    * concatenates the results.
    *  @deprecated use values.flatMap instead
    */
  @deprecated def flatMap[B](f: Value => Iterator[B]): Iterator[B] = elements flatMap f

  /** Returns all values of this enumeration that satisfy the predicate p.
    * The order of values is preserved.
    *  @deprecated use values.filter instead
    */
  @deprecated def filter(p: Value => Boolean): Iterator[Value] = elements filter p
}
