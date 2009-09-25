/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import scala.collection.SetLike
import scala.collection.mutable.{Builder, AddingBuilder, Map, HashMap}
import scala.collection.immutable.{Set, BitSet}
import scala.collection.generic.BuilderFactory

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
 *    WeekDay.iterator filter isWorkingDay foreach println
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
    val name = this.getClass.getName
    var string =
      if (name endsWith "$") name.substring(0, name.length - 1) else name
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(idx2 + 1)
    string
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
      vset = new ValueSet(BitSet.empty ++ (vmap.valuesIterator map (_.id)))
      vsetDefined = true
    }
    vset
  }

  /** The integer to use to identify the next created value. */
  protected var nextId = initial

  /** The string to use to name the next created value. */
  protected var nextName = names.iterator

  /** The highest integer amongst those used to identify values in this
    * enumeration. */
  private var topId = initial

  /** The highest integer amongst those used to identify values in this
    * enumeration. */
  final def maxId = topId

  /** The value of this enumeration with given id `x`
   */
  final def apply(x: Int): Value = vmap(x)

  /** Returns a Value from this Enumeration whose name matches
   * the argument <var>s</var>.
   * You must pass a String* set of names to the constructor,
   * or initialize each Enumeration with Value(String),
   * for valueOf to work.
   * @param s an enumeration name
   * @return <tt>Some(Value)</tt> if an enumeration's name matches <var>s</var>,
   * else <tt>None</tt>
   * Note the change here is intentional. You should know whether
   * a name is in an Enumeration beforehand. If not, just use find on values.
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
     */
    @deprecated("mask32 will be removed")
    def mask32: Int = {
      if (id >= 32) throw new IllegalArgumentException
      1  << id
    }
    /** this enumeration value as an <code>Long</code> bit mask.
     *  @throws IllegalArgumentException if <code>id</code> is greater than 63
     */
    @deprecated("mask64 will be removed")
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
  class ValueSet private[Enumeration] (val ids: BitSet) extends Set[Value] with SetLike[Value, ValueSet] {
    override def empty = ValueSet.empty
    def contains(v: Value) = ids contains (v.id)
    def + (value: Value) = new ValueSet(ids + value.id)
    def - (value: Value) = new ValueSet(ids - value.id)
    def iterator = ids.iterator map Enumeration.this.apply
    override def stringPrefix = Enumeration.this + ".ValueSet"
  }

  /** A factory object for value sets */
  object ValueSet {
    /** The empty value set */
    val empty = new ValueSet(BitSet.empty)
    /** A value set consisting of given elements */
    def apply(elems: Value*): ValueSet = elems.foldLeft(empty)(_ + _)
    /** A builder object for value sets */
    def newBuilder: Builder[Value, ValueSet] = new AddingBuilder(empty)
    /** The implicit builder for value sets */
    implicit def builderFactory: BuilderFactory[Value, ValueSet, ValueSet] = new BuilderFactory[Value, ValueSet, ValueSet] { def apply(from: ValueSet) = newBuilder }
  }

  /** The name of this enumeration. */
  @deprecated("use toString instead") def name = toString

  @deprecated("use withName instead")
  def valueOf(s: String) = values.find(_.toString == s)

  /** A new iterator over all values of this enumeration. */
  @deprecated("use values.iterator instead")
  final def iterator: Iterator[Value] = values.iterator

  /** Apply a function f to all values of this enumeration. */
  @deprecated("use values.foreach instead")
  def foreach(f: Value => Unit): Unit = this.iterator foreach f

  /** Apply a predicate p to all values of this enumeration and return
    * true, iff the predicate yields true for all values.
   */
  @deprecated("use values.forall instead")
  def forall(p: Value => Boolean): Boolean = this.iterator forall p

  /** Apply a predicate p to all values of this enumeration and return
    * true, iff there is at least one value for which p yields true.
    */
  @deprecated("use values.exists instead")
  def exists(p: Value => Boolean): Boolean = this.iterator exists p

  /** Returns an iterator resulting from applying the given function f to each
    * value of this enumeration.
    */
  @deprecated("use values.map instead")
  def map[B](f: Value => B): Iterator[B] = this.iterator map f

  /** Applies the given function f to each value of this enumeration, then
    * concatenates the results.
    */
  @deprecated("use values.flatMap instead")
  def flatMap[B](f: Value => Iterator[B]): Iterator[B] = this.iterator flatMap f

  /** Returns all values of this enumeration that satisfy the predicate p.
    * The order of values is preserved.
    */
  @deprecated("use values.filter instead")
  def filter(p: Value => Boolean): Iterator[Value] = this.iterator filter p
}
