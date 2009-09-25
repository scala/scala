/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.{Builder, AddingBuilder}

/** The canonical factory of <a href="TreeSet.html">TreeSet</a>'s.
 */
object TreeSet extends SortedSetFactory[TreeSet]{

  implicit def implicitBuilder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] = newBuilder[A](ordering)
  def newBuilder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] =
    new AddingBuilder(empty[A](ordering))

  /** The empty set of this type
   */
  def empty[A](implicit ordering: Ordering[A]) = new TreeSet[A]

}

/** This class implements immutable sets using a tree.
 *
 *  @author  Martin Odersky
 *  @version 2.0, 02/01/2007
 */
@serializable @SerialVersionUID(-234066569443569402L)
class TreeSet[A](override val size: Int, t: RedBlack[A]#Tree[Unit])
                (implicit val ordering: Ordering[A])
  extends RedBlack[A] with SortedSet[A] with SortedSetLike[A, TreeSet[A]] {

  override def stringPrefix = "TreeSet"

  def isSmaller(x: A, y: A) = compare(x,y) < 0

  def this()(implicit ordering: Ordering[A]) = this(0, null)(ordering)

  protected val tree: RedBlack[A]#Tree[Unit] = if (size == 0) Empty else t

  private def newSet(s: Int, t: RedBlack[A]#Tree[Unit]) = new TreeSet[A](s, t)

  /** A factory to create empty sets of the same type of keys.
   */
  override def empty = TreeSet.empty

  /** A new TreeSet with the entry added is returned,
   */
  def + (elem: A): TreeSet[A] = {
    val newsize = if (tree.lookup(elem).isEmpty) size + 1 else size
    newSet(newsize, tree.update(elem, ()))
  }

  /** A new TreeSet with the entry added is returned,
   *  assuming that elem is <em>not</em> in the TreeSet.
   */
  def insert(elem: A): TreeSet[A] = {
    assert(tree.lookup(elem).isEmpty)
    newSet(size + 1, tree.update(elem, ()))
  }

  def - (elem:A): TreeSet[A] =
    if (tree.lookup(elem).isEmpty) this
    else newSet(size - 1, tree delete elem)

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param  elem    the element to check for membership.
   *  @return true, iff <code>elem</code> is contained in this set.
   */
  def contains(elem: A): Boolean = !tree.lookup(elem).isEmpty

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[A] = tree.toStream.iterator map (_._1)

  override def toStream: Stream[A] = tree.toStream map (_._1)

  override def foreach[U](f: A =>  U) = tree foreach { (x, y) => f(x) }

  override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = {
    val tree = this.tree.range(from, until)
    newSet(tree.count, tree)
  }
  override def firstKey = tree.first
  override def lastKey = tree.last
}
