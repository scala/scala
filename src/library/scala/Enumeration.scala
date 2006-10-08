/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala


import scala.collection.mutable.{Map, HashMap}

/**
 * <p>The class <code>Enumeration</code> provides the same functionality as the
 * <code>enum</code> construct found in C-like languages like C++ or Java.
 * Here is an example:</p>
 * <pre>
 * <b>object</b> Main <b>extends</b> Application {
 *
 *   <b>object</b> WeekDays <b>extends</b> Enumeration  {
 *     <b>val</b> Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
 *   }
 *
 *   <b>def</b> isWorkingDay(d: WeekDays.Value) =
 *     ! (d == WeekDays.Sat || d == WeekDays.Sun)
 *
 *   WeekDays filter (isWorkingDay) foreach { d =&gt; Console.println(d) }
 * }
 * </pre>
 *
 * @param initial the initial integer value associated with the first element
 * @param names the sequence of element names of the enumeration
 *
 * @author  Matthias Zenger
 * @version 1.0, 10/02/04
 */
abstract class Enumeration(initial: Int, names: String*) {

  def this() = this(0, null)

  def this(names: String*) = this(0, names: _*)

  def name = {
    val cname = compat.Platform.getClassName(this)
    if (cname.endsWith("$"))
      cname.substring(0, cname.length() - 1)
    else if (cname.endsWith("$class"))
      cname.substring(0, cname.length() - 6)
    else
      cname
  }

  /**
   * A mapping between the enumeration value id and the enumeration
   * object.
   */
  private var values: Map[Int, Value] = new HashMap

  /**
   * A cache listing all values of this enumeration.
   */
  private var vcache: List[Value] = null

  private def updateCache: List[Value] =
    if (vcache == null) {
      vcache = values.values.toList.sort((p1, p2) => p1.id < p2.id);
      vcache
    } else
      vcache;

  protected var nextId = initial

  protected var nextName = names.elements

  private var topId = initial

  final def maxId = topId

  /**
   * Returns the enumeration value for the given id.
   */
  final def apply(x: Int): Value = values(x)

  /**
   * Returns all values of this enumeration.
   */
  final def elements: Iterator[Value] = updateCache.elements

  def foreach(f: Value => Unit): Unit = elements foreach f

  def forall(p: Value => Boolean): Boolean = elements forall p

  def exists(p: Value => Boolean): Boolean = elements exists p

  def map[b](f: Value => b): Iterator[b] = elements map f

  def flatMap[b](f: Value => Iterator[b]): Iterator[b] = elements flatMap f

  def filter(p: Value => Boolean): Iterator[Value] = elements filter p

  override def toString(): String = updateCache.mkString("{", ", ", "}")

  protected final def Value: Value =
    new Val(nextId, if (nextName.hasNext) nextName.next else null)

  protected final def Value(i: Int): Value =
    new Val(i, if (nextName.hasNext) nextName.next else null)

  protected final def Value(name: String): Value = new Val(nextId, name)

  protected final def Value(i: Int, name: String): Value = new Val(i, name)

  abstract class Value extends Ordered[Value] {
    def id: Int
    override def compare(that: Value): Int = this.id - that.id
  }

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
      if (name == null) Enumeration.this.name + "(" + i + ")"
      else name
  }
}
