package scala
package collection
package mutable


/** This class implements mutable sets using a hashtable.
  *
  * @author  Matthias Zenger
  * @author  Martin Odersky
  * @since   1
  * @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#hash-tables "Scala's Collection Library overview"]]
  * section on `Hash Tables` for more information.
  *
  * @define Coll `mutable.HashSet`
  * @define coll mutable hash set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
final class HashSet[A]
  extends AbstractSet[A]
    with SetOps[A, HashSet, HashSet[A]]
    with StrictOptimizedIterableOps[A, HashSet, HashSet[A]] {

  @transient private[this] var table = new FlatHashTable[A]

  // Used by scala-java8-compat (private[mutable] erases to public, so Java code can access it)
  private[mutable] def getTable: FlatHashTable[A] = table
  override def iterator: Iterator[A] = table.iterator

  override def iterableFactory: IterableFactory[HashSet] = HashSet

  def addOne(elem: A): this.type = {
    table.addElem(elem)
    this
  }
  def subtractOne(elem: A): this.type = {
    table.removeElem(elem)
    this
  }

  override def add(elem: A): Boolean = table.addElem(elem)

  override def remove(elem: A): Boolean = table.removeElem(elem)

  def clear(): Unit = table.clearTable()

  def contains(elem: A): Boolean = table.containsElem(elem)

  override def knownSize: Int = table.size

  override def size: Int = table.size

  override def isEmpty: Boolean = size == 0

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

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.defaultWriteObject()
    table.serializeTo(out)
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    in.defaultReadObject()
    table = new FlatHashTable[A]
    table.init(in, x => ())
  }

  override protected[this] def stringPrefix = "HashSet"
}

/**
  * $factoryInfo
  * @define Coll `mutable.HashSet`
  * @define coll mutable hash set
  */
@SerialVersionUID(3L)
object HashSet extends IterableFactory[HashSet] {

  def from[B](it: scala.collection.IterableOnce[B]): HashSet[B] = Growable.from(empty[B], it)

  def empty[A]: HashSet[A] = new HashSet[A]

  def newBuilder[A]: Builder[A, HashSet[A]] = new GrowableBuilder(empty[A])

}
