package strawman
package collection
package mutable

import scala.{Any, Boolean, Option, Serializable, SerialVersionUID, Unit}

/** This class implements mutable sets using a hashtable.
  *
  * @author  Matthias Zenger
  * @author  Martin Odersky
  * @version 2.0, 31/12/2006
  * @since   1
  * @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#hash_tables "Scala's Collection Library overview"]]
  * section on `Hash Tables` for more information.
  *
  * @define Coll `mutable.HashSet`
  * @define coll mutable hash set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
@SerialVersionUID(1L)
final class HashSet[A](contents: FlatHashTable.Contents[A])
  extends Set[A]
    with SetOps[A, HashSet, HashSet[A]]
    with Buildable[A, HashSet[A]]
    with Serializable {

  private[this] val table = new FlatHashTable[A]

  def this() = this(null)

  override def iterator(): Iterator[A] = table.iterator

  def iterableFactory = HashSet

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): HashSet[A] = fromIterable(coll)

  protected[this] def newBuilder: Builder[A, HashSet[A]] = HashSet.newBuilder[A]()

  def add(elem: A): this.type = {
    table.addElem(elem)
    this
  }
  def subtract(elem: A): this.type = {
    table.removeElem(elem)
    this
  }

  def clear(): Unit = table.clearTable()

  def contains(elem: A): Boolean = table.containsElem(elem)

  def empty: HashSet[A] = HashSet.empty

  def get(elem: A): Option[A] = table.findEntry(elem)

  override def foreach[U](f: A => U): Unit = {
    var i = 0
    val entries = table.table
    val len = entries.length
    while (i < len) {
      val curEntry = entries(i)
      if (curEntry ne null) f(table.entryToElem(curEntry))
      i += 1
    }
  }

  private def writeObject(s: java.io.ObjectOutputStream): Unit = {
    table.serializeTo(s)
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    table.init(in, x => ())
  }

}

object HashSet extends IterableFactoryWithBuilder[HashSet] {

  def fromIterable[B](it: strawman.collection.Iterable[B]): HashSet[B] = Growable.fromIterable(empty[B], it)

  def empty[A]: HashSet[A] = new HashSet[A]

  def newBuilder[A](): Builder[A, HashSet[A]] = new GrowableBuilder[A, HashSet[A]](empty)

}
