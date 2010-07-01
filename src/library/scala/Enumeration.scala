/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.collection.SetLike
import scala.collection.{ mutable, immutable, generic }
import java.lang.reflect.{ Modifier, Method => JMethod, Field => JField }

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
 *    WeekDay.values filter isWorkingDay foreach println
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
  thisenum =>

  def this() = this(0, null)
  def this(names: String*) = this(0, names: _*)

  /* Note that `readResolve` cannot be private, since otherwise
     the JVM does not invoke it when deserializing subclasses. */
  protected def readResolve(): AnyRef = thisenum.getClass.getField("MODULE$").get()

  /** The name of this enumeration.
   */
  override def toString = (getClass.getName stripSuffix "$" split '.' last) split '$' last

  /** The mapping from the integer used to identify values to the actual
    * values. */
  private val vmap: mutable.Map[Int, Value] = new mutable.HashMap

  /** The cache listing all values of this enumeration. */
  @transient private var vset: ValueSet = null
  @transient private var vsetDefined = false

  /** The mapping from the integer used to identify values to their
    * names. */
  private val nmap: mutable.Map[Int, String] = new mutable.HashMap

  /** The values of this enumeration as a set.
   */
  def values: ValueSet = {
    if (!vsetDefined) {
      vset = new ValueSet(immutable.BitSet.empty ++ (vmap.values map (_.id)))
      vsetDefined = true
    }
    vset
  }

  /** The integer to use to identify the next created value. */
  protected var nextId = initial

  /** The string to use to name the next created value. */
  protected var nextName = names.iterator
  private def nextNameOrElse(orElse: => String) =
    if (nextName.hasNext) nextName.next else orElse

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
   *
   * You can pass a String* set of names to the constructor, or
   * initialize each Enumeration with Value(String). Otherwise, the
   * names are determined automatically through reflection.
   *
   * Note the change here wrt 2.7 is intentional. You should know whether
   * a name is in an Enumeration beforehand. If not, just use find on
   * values.
   *
   * @param  s an Enumeration name
   * @return   the Value of this Enumeration if its name matches <var>s</var>
   * @throws   java.util.NoSuchElementException if no Value with a matching
   *           name is in this Enumeration
   */
  final def withName(s: String): Value = values.find(_.toString == s).get

  /** Creates a fresh value, part of this enumeration. */
  protected final def Value: Value = Value(nextId)

  /** Creates a fresh value, part of this enumeration, identified by the integer
   *  <code>i</code>.
   *
   *  @param i An integer that identifies this value at run-time. It must be
   *           unique amongst all values of the enumeration.
   *  @return  ..
   */
  protected final def Value(i: Int): Value = Value(i, nextNameOrElse(null))

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

  private def populateNameMap() {
    // The list of possible Value methods: 0-args which return a conforming type
    val methods = getClass.getMethods filter (m => m.getParameterTypes.isEmpty && classOf[Value].isAssignableFrom(m.getReturnType))

    methods foreach { m =>
      val name = m.getName
      // invoke method to obtain actual `Value` instance
      val value = m.invoke(this).asInstanceOf[Value]
      // verify that outer points to the correct Enumeration: ticket #3616.
      if (value.outerEnum eq thisenum) {
        val id = Int.unbox(classOf[Val] getMethod "id" invoke value)
        nmap += ((id, name))
      }
    }
  }

  /* Obtains the name for the value with id `i`. If no name is cached
   * in `nmap`, it populates `nmap` using reflection.
   */
  private def nameOf(i: Int): String = synchronized {
    nmap.getOrElse(i, { populateNameMap() ; nmap(i) })
  }

  /** The type of the enumerated values. */
  @serializable
  @SerialVersionUID(7091335633555234129L)
  abstract class Value extends Ordered[Value] {
    /** the id and bit location of this enumeration value */
    def id: Int
    /** a marker so we can tell whose values belong to whom come reflective-naming time */
    private[Enumeration] val outerEnum = thisenum

    override def compare(that: Value): Int = this.id - that.id
    override def equals(other: Any) = other match {
      case that: Enumeration#Value  => (outerEnum eq that.outerEnum) && (id == that.id)
      case _                        => false
    }
    override def hashCode: Int = id.##

    /** this enumeration value as an <code>Int</code> bit mask.
     *  @throws IllegalArgumentException if <code>id</code> is greater than 31
     */
    @deprecated("mask32 will be removed")
    def mask32: Int = {
      if (id >= 32) throw new IllegalArgumentException
      1  << id
    }
    /** this enumeration value as a <code>Long</code> bit mask.
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
   *  overridden to change the enumeration's naming and integer identification
   *  behaviour.
   */
  @serializable
  @SerialVersionUID(0 - 3501153230598116017L)
  protected class Val(i: Int, name: String) extends Value {
    def this(i: Int)        = this(i, nextNameOrElse(i.toString))
    def this(name: String)  = this(nextId, name)
    def this()              = this(nextId)

    assert(!vmap.isDefinedAt(i), "Duplicate id: " + i)
    vmap(i) = this
    vsetDefined = false
    nextId = i + 1
    if (nextId > topId) topId = nextId
    def id = i
    override def toString() =
      if (name != null) name
      else try thisenum.nameOf(i)
      catch { case _: NoSuchElementException => "<Invalid enum: no field for #" + i + ">" }

    protected def readResolve(): AnyRef = {
      val enum = thisenum.readResolve().asInstanceOf[Enumeration]
      if (enum.vmap == null) this
      else enum.vmap(i)
    }
  }

  /** A class for sets of values
   *  Iterating through this set will yield values in increasing order of their ids.
   *  @param   ids   The set of ids of values, organized as a BitSet.
   */
  class ValueSet private[Enumeration] (val ids: immutable.BitSet) extends Set[Value] with SetLike[Value, ValueSet] {
    override def empty = ValueSet.empty
    def contains(v: Value) = ids contains (v.id)
    def + (value: Value) = new ValueSet(ids + value.id)
    def - (value: Value) = new ValueSet(ids - value.id)
    def iterator = ids.iterator map thisenum.apply
    override def stringPrefix = thisenum + ".ValueSet"
  }

  /** A factory object for value sets */
  object ValueSet {
    import mutable.{ Builder, AddingBuilder }
    import generic.CanBuildFrom

    /** The empty value set */
    val empty = new ValueSet(immutable.BitSet.empty)
    /** A value set consisting of given elements */
    def apply(elems: Value*): ValueSet = empty ++ elems
    /** A builder object for value sets */
    def newBuilder: Builder[Value, ValueSet] = new AddingBuilder(empty)
    /** The implicit builder for value sets */
    implicit def canBuildFrom: CanBuildFrom[ValueSet, Value, ValueSet] =
      new CanBuildFrom[ValueSet, Value, ValueSet] {
        def apply(from: ValueSet) = newBuilder
        def apply() = newBuilder
      }
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
