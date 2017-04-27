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
    with FlatHashTable[A]
    with Serializable {

  def this() = this(null)

  override def iterator(): Iterator[A] = super[FlatHashTable].iterator

  protected[this] def fromIterable[B](coll: strawman.collection.Iterable[B]): HashSet[B] =
    HashSet.fromIterable(coll)

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): HashSet[A] = fromIterable(coll)

  protected[this] def newBuilder: Builder[A, HashSet[A]] = new GrowableBuilder[A, HashSet[A]](empty)

  def result: HashSet[A] = this

  def add(elem: A): this.type = {
    addElem(elem)
    this
  }
  def subtract(elem: A): this.type = {
    removeElem(elem)
    this
  }

  def clear(): Unit = {
    clearTable()
  }

  def contains(elem: A): Boolean = containsElem(elem)

  def empty: HashSet[A] = HashSet.empty

  def get(elem: A): Option[A] = scala.Predef.???

  override def foreach[U](f: A => U): Unit = {
    var i = 0
    val len = table.length
    while (i < len) {
      val curEntry = table(i)
      if (curEntry ne null) f(entryToElem(curEntry))
      i += 1
    }
  }

}

object HashSet extends IterableFactory[HashSet] {

  def fromIterable[B](it: strawman.collection.Iterable[B]): HashSet[B] = {
    val result = new HashSet[B]
    for (elem <- it) {
      result += elem
    }
    result
  }

  def empty[A]: HashSet[A] = new HashSet[A]

}
