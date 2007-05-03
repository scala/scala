/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import scala.collection.mutable.{Map, HashMap}

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
 *    <b>object</b> WeekDays <b>extends</b> Enumeration {
 *      <b>val</b> Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
 *    }
 *
 *    <b>def</b> isWorkingDay(d: WeekDays.Value) =
 *      ! (d == WeekDays.Sat || d == WeekDays.Sun)
 *
 *    WeekDays filter (isWorkingDay) foreach { d =&gt; Console.println(d) }
 *  }</pre>
 *
 *  @param initial The initial value from which to count the integers that
 *                 identifies values at run-time.
 *  @param names   The sequence of names to give to this enumeration's values.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/02/04
 */
abstract class Enumeration(initial: Int, names: String*) {

  def this() = this(0, null)

  def this(names: String*) = this(0, names: _*)

  /** The name of this enumeration.  */
  def name = {
    val cname = this.getClass().getName()
    if (cname.endsWith("$"))
      cname.substring(0, cname.length() - 1)
    else if (cname.endsWith("$class"))
      cname.substring(0, cname.length() - 6)
    else
      cname
  }

  /** The mapping from the integer used to identifying values to the actual
    * values. */
  private val values: Map[Int, Value] = new HashMap

  /** The cache listing all values of this enumeration. */
  private var vcache: List[Value] = null

  private def updateCache: List[Value] =
    if (vcache eq null) {
      vcache = values.values.toList.sort((p1, p2) => p1.id < p2.id);
      vcache
    } else
      vcache;

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

  /** The value in this enumeration identified by integer <code>x</code>. */
  final def apply(x: Int): Value = values(x)

  /** A new iterator over all values of this enumeration. */
  final def elements: Iterator[Value] = updateCache.elements

  /** Apply a function f to all values of this enumeration. */
  def foreach(f: Value => Unit): Unit = elements foreach f

  /** Apply a predicate p to all values of this enumeration and return
    * true, iff the predicate yields true for all values. */
  def forall(p: Value => Boolean): Boolean = elements forall p

  /** Apply a predicate p to all values of this enumeration and return
    * true, iff there is at least one value for which p yields true. */
  def exists(p: Value => Boolean): Boolean = elements exists p

  /** Returns an iterator resulting from applying the given function f to each
    * value of this enumeration. */
  def map[b](f: Value => b): Iterator[b] = elements map f

  /** Applies the given function f to each value of this enumeration, then
    * concatenates the results. */
  def flatMap[b](f: Value => Iterator[b]): Iterator[b] = elements flatMap f

  /** Returns all values of this enumeration that satisfy the predicate p.
    * The order of values is preserved. */
  def filter(p: Value => Boolean): Iterator[Value] = elements filter p

  override def toString(): String = updateCache.mkString("{", ", ", "}")

  /** Creates a fresh value, part of this enumeration. */
  protected final def Value: Value =
    new Val(nextId, if (nextName.hasNext) nextName.next else null)

  /** Creates a fresh value, part of this enumeration, identified by the integer
   *  <code>i</code>.
   *
   *  @param i An integer that identifies this value at run-time. It must be
   *           unique amongst all values of the enumeration.
   *  @return  ..
   */
  protected final def Value(i: Int): Value =
    new Val(i, if (nextName.hasNext) nextName.next else null)

  /** Creates a fresh value, part of this enumeration, called <code>name</code>.
   *
   *  @param name A human-readable name for that value.
   */
  protected final def Value(name: String): Value = new Val(nextId, name)

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
  abstract class Value extends Ordered[Value] {
    /** the id and bit location of this enumeration value */
    def id: Int
    override def compare(that: Value): Int = this.id - that.id
    /** this enumeration value as an <code>Int</code> bit mask.
     *  @throws IllegalArgumentException if <code>id</code> is greater than 31
     */
    def mask32: Int = {
      if (id >= 32) throw new IllegalArgumentException
      1  << id
    }
    /** this enumeration value as an <code>Long</code> bit mask.
     *  @throws IllegalArgumentException if <code>id</code> is greater than 63
     */
    def mask64: Long = {
      if (id >= 64) throw new IllegalArgumentException
      1L << id
    }
  }

  /** A class implementing the <a href="Enumeration.Value.html"
   *  target="contentFrame"><code>Value</code></a> type. This class can be
   *  overriden to change the enumeration's naming and integer identification
   *  behaviour.
   */
  protected class Val(i: Int, name: String) extends Value {
    def this(i: Int) =
      this(i, if (nextName.hasNext) nextName.next else i.toString())
    def this(name: String) = this(nextId, name)
    def this() =
      this(nextId, if (nextName.hasNext) nextName.next else nextId.toString())
    assert(!values.isDefinedAt(i))
    values(i) = this
    nextId = i + 1
    if (nextId > topId)
      topId = nextId
    def id = i
    override def toString() =
      if (name eq null) Enumeration.this.name + "(" + i + ")"
      else name
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
   */
  abstract class SetXX extends collection.immutable.Set[Value] {

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
    def size = {
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
        val ret = values(bit)
        shift
        ret
      }
    }
    def empty[B]: scala.collection.immutable.Set[B] = new scala.collection.immutable.HashSet[B];
  }

  /** An enumeration bit set that can handle enumeration values with ids up
   *  to 31 in an <code>Int</code>.
   */
  class Set32(val underlying : Int) extends SetXX {
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
    def contains(value : Value) = (underlying & value.mask32) != 0
    def |(  set :   Set32) = new Set32(underlying |   set.underlying)
    def |(value : Value) = new Set32(underlying | value.mask32)
    def &~(value : Value) = new Set32(underlying & (~value.mask32))
    def &(set : Set32) = new Set32(underlying & set.underlying)
  }

  /** create an empty 32 bit enumeration set */
  def Set32 = new Set32

  /** create a bit enumeration set according ot underlying */
  def Set32(underlying : Int) = new Set32(underlying)

  /** An enumeration bit set that can handle enumeration values with ids up
   *  to 63 in a <code>Long</code>.
   */
  class Set64(val underlying : Long) extends SetXX {
    def this() = this(0)
    type Underlying = Long
    type TSet = Set64
    def underlyingAsLong = underlying
    def contains(value : Value) = (underlying & value.mask64) != 0
    def |(  set :   Set64) = new Set64(underlying |   set.underlying)
    def |(value : Value) = new Set64(underlying | value.mask64)
    def &~(value : Value) = new Set64(underlying & (~value.mask64))
    def &(set : Set64) = new Set64(underlying & set.underlying)
  }

  /** create an empty 64 bit enumeration set */
  def Set64 = new Set64

  /** create a bit enumeration set according ot underlying */
  def Set64(underlying: Long) = new Set64(underlying)

  /** used to reverse engineer bit locations from pre-defined bit masks */
  def maskToBit(n: Long) = {
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
}
