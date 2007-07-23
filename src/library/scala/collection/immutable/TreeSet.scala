/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

/** The canonical factory of <a href="TreeSet.html">TreeSet</a>'s. */

object TreeSet {

  /** The empty set of this type
   */
  def empty[A <% Ordered[A]] = new TreeSet[A]

  /** The canonical factory for this type
   */
  def apply[A <% Ordered[A]](elems: A*) = empty[A] ++ elems
}

/** This class implements immutable sets using a tree.
 *
 *  @author  Martin Odersky
 *  @version 2.0, 02/01/2007
 */

@serializable
class TreeSet[A <% Ordered[A]](val size: Int, t: RedBlack[A]#Tree[Unit])
  extends RedBlack[A] with SortedSet[A] {

  def isSmaller(x: A, y: A) = x < y

  def this() = this(0, null)

  protected val tree: RedBlack[A]#Tree[Unit] = if (size == 0) Empty else t

  private def newSet(s: Int, t: RedBlack[A]#Tree[Unit]) = new TreeSet[A](s, t)

  /** A factory to create empty maps of the same type of keys.
   */
  def empty[B]: Set[B] = ListSet.empty[B]

  /** A new TreeSet with the entry added is returned,
   */
  def + (elem: A): TreeSet[A] = {
    val newsize = if (tree.lookup(elem).isEmpty) size + 1 else size
    newSet(newsize, tree.update(elem, ()))
  }

  /** A new TreeSet with the entry added is returned,
   *  assuming that elem is <em>not</em> in the TreeSet.
   */
  def insert (elem: A): TreeSet[A] = {
    assert(tree.lookup(elem).isEmpty)
    newSet(size + 1, tree.update(elem, ()))
  }

  def - (elem:A): TreeSet[A] =
    if (tree.lookup(elem).isEmpty) this
    else newSet(size - 1, tree.delete(elem))

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
  def elements: Iterator[A] = tree.elements.elements map (_._1)

  def elementsSlow = tree.elementsSlow map (_._1)

  override def foreach(f: A => Unit) {
    tree.visit[Unit](())((unit0, y, unit1) => Tuple2(true, f(y)))
  }

  override def forall(f: A => Boolean): Boolean =
    tree.visit[Boolean](true)((input, a, unit) => f(a) match {
    case ret if input => Tuple2(ret, ret)
    })._2

  override def exists(f: A => Boolean): Boolean =
    tree.visit[Boolean](false)((input, a, unit) => f(a) match {
    case ret if !input => Tuple2(!ret, ret)
    })._2

   override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = {
     val tree = this.tree.range(from, until)
     newSet(tree.count, tree)
   }
   override def firstKey = tree.first
   override def lastKey = tree.last
   override def compare(a0: A, a1: A) = a0.compare(a1)
}
