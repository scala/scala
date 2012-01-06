/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import annotation.tailrec
import annotation.meta.getter

/** An object containing the RedBlack tree implementation used by for `TreeMaps` and `TreeSets`.
 *
 *  Implementation note: since efficiency is important for data structures this implementation
 *  uses <code>null</code> to represent empty trees. This also means pattern matching cannot
 *  easily be used. The API represented by the RedBlack object tries to hide these optimizations
 *  behind a reasonably clean API.
 *
 *  @since 2.3
 */
private[immutable]
object RedBlack {

  def isBlack(tree: Node[_, _]) = (tree eq null) || isBlackNode(tree)
  def isRedNode(tree: Node[_, _]) = tree.isInstanceOf[RedNode[_, _]]
  def isBlackNode(tree: Node[_, _]) = tree.isInstanceOf[BlackNode[_, _]]

  def isEmpty(tree: Node[_, _]): Boolean = tree eq null

  def contains[A](tree: Node[A, _], x: A)(implicit ordering: Ordering[A]): Boolean = lookup(tree, x) ne null
  def get[A, B](tree: Node[A, B], x: A)(implicit ordering: Ordering[A]): Option[B] = lookup(tree, x) match {
    case null => None
    case tree => Some(tree.value)
  }

  @tailrec
  def lookup[A, B](tree: Node[A, B], x: A)(implicit ordering: Ordering[A]): Node[A, B] = if (tree eq null) null else {
    val cmp = ordering.compare(x, tree.key)
    if (cmp < 0) lookup(tree.left, x)
    else if (cmp > 0) lookup(tree.right, x)
    else tree
  }

  def count(tree: Node[_, _]) = if (tree eq null) 0 else tree.count
  def update[A, B, B1 >: B](tree: Node[A, B], k: A, v: B1)(implicit ordering: Ordering[A]): Node[A, B1] = blacken(upd(tree, k, v))
  def delete[A, B](tree: Node[A, B], k: A)(implicit ordering: Ordering[A]): Node[A, B] = blacken(del(tree, k))
  def range[A, B](tree: Node[A, B], from: Option[A], until: Option[A])(implicit ordering: Ordering[A]): Node[A, B] = blacken(rng(tree, from, until))

  def smallest[A, B](tree: Node[A, B]): Node[A, B] = {
    if (tree eq null) throw new NoSuchElementException("empty map")
    var result = tree
    while (result.left ne null) result = result.left
    result
  }
  def greatest[A, B](tree: Node[A, B]): Node[A, B] = {
    if (tree eq null) throw new NoSuchElementException("empty map")
    var result = tree
    while (result.right ne null) result = result.right
    result
  }

  def foreach[A, B, U](tree: Node[A, B], f: ((A, B)) => U): Unit = if (tree ne null) {
    if (tree.left ne null) foreach(tree.left, f)
    f((tree.key, tree.value))
    if (tree.right ne null) foreach(tree.right, f)
  }
  def foreachKey[A, U](tree: Node[A, _], f: A => U): Unit = if (tree ne null) {
    if (tree.left ne null) foreachKey(tree.left, f)
    f(tree.key)
    if (tree.right ne null) foreachKey(tree.right, f)
  }

  def iterator[A, B](tree: Node[A, B]): Iterator[(A, B)] = new EntriesIterator(tree)
  def keysIterator[A, _](tree: Node[A, _]): Iterator[A] = new KeysIterator(tree)
  def valuesIterator[_, B](tree: Node[_, B]): Iterator[B] = new ValuesIterator(tree)

  @tailrec
  def nth[A, B](tree: Node[A, B], n: Int): Node[A, B] = {
    val count = RedBlack.count(tree.left)
    if (n < count) nth(tree.left, n)
    else if (n > count) nth(tree.right, n - count - 1)
    else tree
  }

  private def blacken[A, B](t: Node[A, B]): Node[A, B] = if (t eq null) null else t.black

  private def mkNode[A, B](isBlack: Boolean, k: A, v: B, l: Node[A, B], r: Node[A, B]) =
    if (isBlack) BlackNode(k, v, l, r) else RedNode(k, v, l, r)

  private[this] def balanceLeft[A, B, B1 >: B](isBlack: Boolean, z: A, zv: B, l: Node[A, B1], d: Node[A, B1]): Node[A, B1] = {
    if (isRedNode(l) && isRedNode(l.left))
      RedNode(l.key, l.value, BlackNode(l.left.key, l.left.value, l.left.left, l.left.right), BlackNode(z, zv, l.right, d))
    else if (isRedNode(l) && isRedNode(l.right))
      RedNode(l.right.key, l.right.value, BlackNode(l.key, l.value, l.left, l.right.left), BlackNode(z, zv, l.right.right, d))
    else
      mkNode(isBlack, z, zv, l, d)
  }
  private[this] def balanceRight[A, B, B1 >: B](isBlack: Boolean, x: A, xv: B, a: Node[A, B1], r: Node[A, B1]): Node[A, B1] = {
    if (isRedNode(r) && isRedNode(r.left))
      RedNode(r.left.key, r.left.value, BlackNode(x, xv, a, r.left.left), BlackNode(r.key, r.value, r.left.right, r.right))
    else if (isRedNode(r) && isRedNode(r.right))
      RedNode(r.key, r.value, BlackNode(x, xv, a, r.left), BlackNode(r.right.key, r.right.value, r.right.left, r.right.right))
    else
      mkNode(isBlack, x, xv, a, r)
  }
  private[this] def upd[A, B, B1 >: B](tree: Node[A, B], k: A, v: B1)(implicit ordering: Ordering[A]): Node[A, B1] = if (tree eq null) {
    RedNode(k, v, null, null)
  } else {
    val cmp = ordering.compare(k, tree.key)
    if (cmp < 0) balanceLeft(tree.isBlack, tree.key, tree.value, upd(tree.left, k, v), tree.right)
    else if (cmp > 0) balanceRight(tree.isBlack, tree.key, tree.value, tree.left, upd(tree.right, k, v))
    else mkNode(tree.isBlack, k, v, tree.left, tree.right)
  }

    // Based on Stefan Kahrs' Haskell version of Okasaki's Red&Black Trees
    // http://www.cse.unsw.edu.au/~dons/data/RedBlackNode.html
  private[this] def del[A, B](tree: Node[A, B], k: A)(implicit ordering: Ordering[A]): Node[A, B] = if (tree eq null) null else {
    def balance(x: A, xv: B, tl: Node[A, B], tr: Node[A, B]) = if (isRedNode(tl)) {
      if (isRedNode(tr)) {
        RedNode(x, xv, tl.black, tr.black)
      } else if (isRedNode(tl.left)) {
        RedNode(tl.key, tl.value, tl.left.black, BlackNode(x, xv, tl.right, tr))
      } else if (isRedNode(tl.right)) {
        RedNode(tl.right.key, tl.right.value, BlackNode(tl.key, tl.value, tl.left, tl.right.left), BlackNode(x, xv, tl.right.right, tr))
      } else {
        BlackNode(x, xv, tl, tr)
      }
    } else if (isRedNode(tr)) {
      if (isRedNode(tr.right)) {
        RedNode(tr.key, tr.value, BlackNode(x, xv, tl, tr.left), tr.right.black)
      } else if (isRedNode(tr.left)) {
        RedNode(tr.left.key, tr.left.value, BlackNode(x, xv, tl, tr.left.left), BlackNode(tr.key, tr.value, tr.left.right, tr.right))
      } else {
        BlackNode(x, xv, tl, tr)
      }
    } else {
      BlackNode(x, xv, tl, tr)
    }
    def subl(t: Node[A, B]) =
      if (t.isInstanceOf[BlackNode[_, _]]) t.red
      else sys.error("Defect: invariance violation; expected black, got "+t)

    def balLeft(x: A, xv: B, tl: Node[A, B], tr: Node[A, B]) = if (isRedNode(tl)) {
      RedNode(x, xv, tl.black, tr)
    } else if (isBlackNode(tr)) {
      balance(x, xv, tl, tr.red)
    } else if (isRedNode(tr) && isBlackNode(tr.left)) {
      RedNode(tr.left.key, tr.left.value, BlackNode(x, xv, tl, tr.left.left), balance(tr.key, tr.value, tr.left.right, subl(tr.right)))
    } else {
      sys.error("Defect: invariance violation")
    }
    def balRight(x: A, xv: B, tl: Node[A, B], tr: Node[A, B]) = if (isRedNode(tr)) {
      RedNode(x, xv, tl, tr.black)
    } else if (isBlackNode(tl)) {
      balance(x, xv, tl.red, tr)
    } else if (isRedNode(tl) && isBlackNode(tl.right)) {
      RedNode(tl.right.key, tl.right.value, balance(tl.key, tl.value, subl(tl.left), tl.right.left), BlackNode(x, xv, tl.right.right, tr))
    } else {
      sys.error("Defect: invariance violation")
    }
    def delLeft = if (isBlackNode(tree.left)) balLeft(tree.key, tree.value, del(tree.left, k), tree.right) else RedNode(tree.key, tree.value, del(tree.left, k), tree.right)
    def delRight = if (isBlackNode(tree.right)) balRight(tree.key, tree.value, tree.left, del(tree.right, k)) else RedNode(tree.key, tree.value, tree.left, del(tree.right, k))
    def append(tl: Node[A, B], tr: Node[A, B]): Node[A, B] = if (tl eq null) {
      tr
    } else if (tr eq null) {
      tl
    } else if (isRedNode(tl) && isRedNode(tr)) {
      val bc = append(tl.right, tr.left)
      if (isRedNode(bc)) {
        RedNode(bc.key, bc.value, RedNode(tl.key, tl.value, tl.left, bc.left), RedNode(tr.key, tr.value, bc.right, tr.right))
      } else {
        RedNode(tl.key, tl.value, tl.left, RedNode(tr.key, tr.value, bc, tr.right))
      }
    } else if (isBlackNode(tl) && isBlackNode(tr)) {
      val bc = append(tl.right, tr.left)
      if (isRedNode(bc)) {
        RedNode(bc.key, bc.value, BlackNode(tl.key, tl.value, tl.left, bc.left), BlackNode(tr.key, tr.value, bc.right, tr.right))
      } else {
        balLeft(tl.key, tl.value, tl.left, BlackNode(tr.key, tr.value, bc, tr.right))
      }
    } else if (isRedNode(tr)) {
      RedNode(tr.key, tr.value, append(tl, tr.left), tr.right)
    } else if (isRedNode(tl)) {
      RedNode(tl.key, tl.value, tl.left, append(tl.right, tr))
    } else {
      sys.error("unmatched tree on append: " + tl + ", " + tr)
    }

    val cmp = ordering.compare(k, tree.key)
    if (cmp < 0) delLeft
    else if (cmp > 0) delRight
    else append(tree.left, tree.right)
  }

  private[this] def rng[A, B](tree: Node[A, B], from: Option[A], until: Option[A])(implicit ordering: Ordering[A]): Node[A, B] = {
    if (tree eq null) return null
    if (from == None && until == None) return tree
    if (from != None && ordering.lt(tree.key, from.get)) return rng(tree.right, from, until);
    if (until != None && ordering.lteq(until.get, tree.key)) return rng(tree.left, from, until);
    val newLeft = rng(tree.left, from, None)
    val newRight = rng(tree.right, None, until)
    if ((newLeft eq tree.left) && (newRight eq tree.right)) tree
    else if (newLeft eq null) upd(newRight, tree.key, tree.value);
    else if (newRight eq null) upd(newLeft, tree.key, tree.value);
    else rebalance(tree, newLeft, newRight)
  }

  // The zipper returned might have been traversed left-most (always the left child)
  // or right-most (always the right child). Left trees are traversed right-most,
  // and right trees are traversed leftmost.

  // Returns the zipper for the side with deepest black nodes depth, a flag
  // indicating whether the trees were unbalanced at all, and a flag indicating
  // whether the zipper was traversed left-most or right-most.

  // If the trees were balanced, returns an empty zipper
  private[this] def compareDepth[A, B](left: Node[A, B], right: Node[A, B]): (List[Node[A, B]], Boolean, Boolean, Int) = {
    // Once a side is found to be deeper, unzip it to the bottom
    def unzip(zipper: List[Node[A, B]], leftMost: Boolean): List[Node[A, B]] = {
      val next = if (leftMost) zipper.head.left else zipper.head.right
      next match {
        case null => zipper
        case node => unzip(node :: zipper, leftMost)
      }
    }

    // Unzip left tree on the rightmost side and right tree on the leftmost side until one is
    // found to be deeper, or the bottom is reached
    def unzipBoth(left: Node[A, B],
                  right: Node[A, B],
                  leftZipper: List[Node[A, B]],
                  rightZipper: List[Node[A, B]],
                  smallerDepth: Int): (List[Node[A, B]], Boolean, Boolean, Int) = {
      if (isBlackNode(left) && isBlackNode(right)) {
        unzipBoth(left.right, right.left, left :: leftZipper, right :: rightZipper, smallerDepth + 1)
      } else if (isRedNode(left) && isRedNode(right)) {
        unzipBoth(left.right, right.left, left :: leftZipper, right :: rightZipper, smallerDepth)
      } else if (isRedNode(right)) {
        unzipBoth(left, right.left, leftZipper, right :: rightZipper, smallerDepth)
      } else if (isRedNode(left)) {
        unzipBoth(left.right, right, left :: leftZipper, rightZipper, smallerDepth)
      } else if ((left eq null) && (right eq null)) {
        (Nil, true, false, smallerDepth)
      } else if ((left eq null) && isBlackNode(right)) {
        val leftMost = true
        (unzip(right :: rightZipper, leftMost), false, leftMost, smallerDepth)
      } else if (isBlackNode(left) && (right eq null)) {
        val leftMost = false
        (unzip(left :: leftZipper, leftMost), false, leftMost, smallerDepth)
      } else {
        sys.error("unmatched trees in unzip: " + left + ", " + right)
      }
    }
    unzipBoth(left, right, Nil, Nil, 0)
  }
  private[this] def rebalance[A, B](tree: Node[A, B], newLeft: Node[A, B], newRight: Node[A, B]) = {
    // This is like drop(n-1), but only counting black nodes
    def  findDepth(zipper: List[Node[A, B]], depth: Int): List[Node[A, B]] = zipper match {
      case head :: tail if isBlackNode(head) =>
        if (depth == 1) zipper else findDepth(tail, depth - 1)
      case _ :: tail => findDepth(tail, depth)
      case Nil => sys.error("Defect: unexpected empty zipper while computing range")
    }

    // Blackening the smaller tree avoids balancing problems on union;
    // this can't be done later, though, or it would change the result of compareDepth
    val blkNewLeft = blacken(newLeft)
    val blkNewRight = blacken(newRight)
    val (zipper, levelled, leftMost, smallerDepth) = compareDepth(blkNewLeft, blkNewRight)

    if (levelled) {
      BlackNode(tree.key, tree.value, blkNewLeft, blkNewRight)
    } else {
      val zipFrom = findDepth(zipper, smallerDepth)
      val union = if (leftMost) {
        RedNode(tree.key, tree.value, blkNewLeft, zipFrom.head)
      } else {
        RedNode(tree.key, tree.value, zipFrom.head, blkNewRight)
      }
      val zippedTree = zipFrom.tail.foldLeft(union: Node[A, B]) { (tree, node) =>
        if (leftMost)
          balanceLeft(node.isBlack, node.key, node.value, tree, node.right)
        else
          balanceRight(node.isBlack, node.key, node.value, node.left, tree)
      }
      zippedTree
    }
  }

  /*
   * Forcing direct fields access using the @inline annotation helps speed up
   * various operations (especially smallest/greatest and update/delete).
   *
   * Unfortunately the direct field access is not guaranteed to work (but
   * works on the current implementation of the Scala compiler).
   *
   * An alternative is to implement the these classes using plain old Java code...
   */
  sealed abstract class Node[A, +B](
    @(inline @getter) final val key: A,
    @(inline @getter) final val value: B,
    @(inline @getter) final val left: Node[A, B],
    @(inline @getter) final val right: Node[A, B])
  extends Serializable {
    final val count: Int = 1 + RedBlack.count(left) + RedBlack.count(right)
    def isBlack: Boolean
    def black: Node[A, B]
    def red: Node[A, B]
  }
  final class RedNode[A, +B](key: A,
                             value: B,
                             left: Node[A, B],
                             right: Node[A, B]) extends Node[A, B](key, value, left, right) {
    override def isBlack = false
    override def black = BlackNode(key, value, left, right)
    override def red = this
    override def toString = "RedNode(" + key + ", " + value + ", " + left + ", " + right + ")"
  }
  final class BlackNode[A, +B](key: A,
                               value: B,
                               left: Node[A, B],
                               right: Node[A, B]) extends Node[A, B](key, value, left, right) {
    override def isBlack = true
    override def black = this
    override def red = RedNode(key, value, left, right)
    override def toString = "BlackNode(" + key + ", " + value + ", " + left + ", " + right + ")"
  }

  object RedNode {
    @inline def apply[A, B](key: A, value: B, left: Node[A, B], right: Node[A, B]) = new RedNode(key, value, left, right)
    def unapply[A, B](t: RedNode[A, B]) = Some((t.key, t.value, t.left, t.right))
  }
  object BlackNode {
    @inline def apply[A, B](key: A, value: B, left: Node[A, B], right: Node[A, B]) = new BlackNode(key, value, left, right)
    def unapply[A, B](t: BlackNode[A, B]) = Some((t.key, t.value, t.left, t.right))
  }

  private[this] abstract class TreeIterator[A, B, R](tree: Node[A, B]) extends Iterator[R] {
    protected[this] def nextResult(tree: Node[A, B]): R

    override def hasNext: Boolean = next ne null

    override def next: R = next match {
      case null =>
        throw new NoSuchElementException("next on empty iterator")
      case tree =>
        next = findNext(tree.right)
        nextResult(tree)
    }

    @tailrec
    private[this] def findNext(tree: Node[A, B]): Node[A, B] = {
      if (tree eq null) popPath()
      else if (tree.left eq null) tree
      else {
        pushPath(tree)
        findNext(tree.left)
      }
    }

    private[this] def pushPath(tree: Node[A, B]) {
      try {
        path(index) = tree
        index += 1
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          /*
           * Either the tree became unbalanced or we calculated the maximum height incorrectly.
           * To avoid crashing the iterator we expand the path array. Obviously this should never
           * happen...
           *
           * An exception handler is used instead of an if-condition to optimize the normal path.
           * This makes a large difference in iteration speed!
           */
          assert(index >= path.length)
          path :+= null
          pushPath(tree)
      }
    }
    private[this] def popPath(): Node[A, B] = if (index == 0) null else {
      index -= 1
      path(index)
    }

    private[this] var path = if (tree eq null) null else {
      /*
       * According to "Ralf Hinze. Constructing red-black trees" [http://www.cs.ox.ac.uk/ralf.hinze/publications/#P5]
       * the maximum height of a red-black tree is 2*log_2(n + 2) - 2.
       *
       * According to {@see Integer#numberOfLeadingZeros} ceil(log_2(n)) = (32 - Integer.numberOfLeadingZeros(n - 1))
       *
       * We also don't store the deepest nodes in the path so the maximum path length is further reduced by one.
       */
      val maximumHeight = 2 * (32 - Integer.numberOfLeadingZeros(tree.count + 2 - 1)) - 2 - 1
      new Array[Node[A, B]](maximumHeight)
    }
    private[this] var index = 0
    private[this] var next: Node[A, B] = findNext(tree)
  }

  private[this] class EntriesIterator[A, B](tree: Node[A, B]) extends TreeIterator[A, B, (A, B)](tree) {
    override def nextResult(tree: Node[A, B]) = (tree.key, tree.value)
  }

  private[this] class KeysIterator[A, B](tree: Node[A, B]) extends TreeIterator[A, B, A](tree) {
    override def nextResult(tree: Node[A, B]) = tree.key
  }

  private[this] class ValuesIterator[A, B](tree: Node[A, B]) extends TreeIterator[A, B, B](tree) {
    override def nextResult(tree: Node[A, B]) = tree.value
  }
}


/** Old base class that was used by previous implementations of `TreeMaps` and `TreeSets`.
 *
 *  Deprecated due to various performance bugs (see [[https://issues.scala-lang.org/browse/SI-5331 SI-5331]] for more information).
 *
 *  @since 2.3
 */
@deprecated("use `TreeMap` or `TreeSet` instead", "2.10")
@SerialVersionUID(8691885935445612921L)
abstract class RedBlack[A] extends Serializable {

  def isSmaller(x: A, y: A): Boolean

  private def blacken[B](t: Tree[B]): Tree[B] = t match {
    case RedTree(k, v, l, r) => BlackTree(k, v, l, r)
    case t => t
  }
  private def mkTree[B](isBlack: Boolean, k: A, v: B, l: Tree[B], r: Tree[B]) =
    if (isBlack) BlackTree(k, v, l, r) else RedTree(k, v, l, r)

  abstract class Tree[+B] extends Serializable {
    def isEmpty: Boolean
    def isBlack: Boolean
    def lookup(x: A): Tree[B]
    def update[B1 >: B](k: A, v: B1): Tree[B1] = blacken(upd(k, v))
    def delete(k: A): Tree[B] = blacken(del(k))
    def range(from: Option[A], until: Option[A]): Tree[B] = blacken(rng(from, until))
    def foreach[U](f: (A, B) =>  U)
    def toStream: Stream[(A,B)]
    def iterator: Iterator[(A, B)]
    def upd[B1 >: B](k: A, v: B1): Tree[B1]
    def del(k: A): Tree[B]
    def smallest: NonEmpty[B]
    def rng(from: Option[A], until: Option[A]): Tree[B]
    def first : A
    def last : A
    def count : Int
  }
  abstract class NonEmpty[+B] extends Tree[B] with Serializable {
    def isEmpty = false
    def key: A
    def value: B
    def left: Tree[B]
    def right: Tree[B]
    def lookup(k: A): Tree[B] =
      if (isSmaller(k, key)) left.lookup(k)
      else if (isSmaller(key, k)) right.lookup(k)
      else this
    private[this] def balanceLeft[B1 >: B](isBlack: Boolean, z: A, zv: B, l: Tree[B1], d: Tree[B1])/*: NonEmpty[B1]*/ = l match {
      case RedTree(y, yv, RedTree(x, xv, a, b), c) =>
        RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
      case RedTree(x, xv, a, RedTree(y, yv, b, c)) =>
        RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
      case _ =>
        mkTree(isBlack, z, zv, l, d)
    }
    private[this] def balanceRight[B1 >: B](isBlack: Boolean, x: A, xv: B, a: Tree[B1], r: Tree[B1])/*: NonEmpty[B1]*/ = r match {
      case RedTree(z, zv, RedTree(y, yv, b, c), d) =>
        RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
      case RedTree(y, yv, b, RedTree(z, zv, c, d)) =>
        RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
      case _ =>
        mkTree(isBlack, x, xv, a, r)
    }
    def upd[B1 >: B](k: A, v: B1): Tree[B1] = {
      if (isSmaller(k, key)) balanceLeft(isBlack, key, value, left.upd(k, v), right)
      else if (isSmaller(key, k)) balanceRight(isBlack, key, value, left, right.upd(k, v))
      else mkTree(isBlack, k, v, left, right)
    }
    // Based on Stefan Kahrs' Haskell version of Okasaki's Red&Black Trees
    // http://www.cse.unsw.edu.au/~dons/data/RedBlackTree.html
    def del(k: A): Tree[B] = {
      def balance(x: A, xv: B, tl: Tree[B], tr: Tree[B]) = (tl, tr) match {
        case (RedTree(y, yv, a, b), RedTree(z, zv, c, d)) =>
          RedTree(x, xv, BlackTree(y, yv, a, b), BlackTree(z, zv, c, d))
        case (RedTree(y, yv, RedTree(z, zv, a, b), c), d) =>
          RedTree(y, yv, BlackTree(z, zv, a, b), BlackTree(x, xv, c, d))
        case (RedTree(y, yv, a, RedTree(z, zv, b, c)), d) =>
          RedTree(z, zv, BlackTree(y, yv, a, b), BlackTree(x, xv, c, d))
        case (a, RedTree(y, yv, b, RedTree(z, zv, c, d))) =>
          RedTree(y, yv, BlackTree(x, xv, a, b), BlackTree(z, zv, c, d))
        case (a, RedTree(y, yv, RedTree(z, zv, b, c), d)) =>
          RedTree(z, zv, BlackTree(x, xv, a, b), BlackTree(y, yv, c, d))
        case (a, b) =>
          BlackTree(x, xv, a, b)
      }
      def subl(t: Tree[B]) = t match {
        case BlackTree(x, xv, a, b) => RedTree(x, xv, a, b)
        case _ => sys.error("Defect: invariance violation; expected black, got "+t)
      }
      def balLeft(x: A, xv: B, tl: Tree[B], tr: Tree[B]) = (tl, tr) match {
        case (RedTree(y, yv, a, b), c) =>
          RedTree(x, xv, BlackTree(y, yv, a, b), c)
        case (bl, BlackTree(y, yv, a, b)) =>
          balance(x, xv, bl, RedTree(y, yv, a, b))
        case (bl, RedTree(y, yv, BlackTree(z, zv, a, b), c)) =>
          RedTree(z, zv, BlackTree(x, xv, bl, a), balance(y, yv, b, subl(c)))
        case _ => sys.error("Defect: invariance violation at "+right)
      }
      def balRight(x: A, xv: B, tl: Tree[B], tr: Tree[B]) = (tl, tr) match {
        case (a, RedTree(y, yv, b, c)) =>
          RedTree(x, xv, a, BlackTree(y, yv, b, c))
        case (BlackTree(y, yv, a, b), bl) =>
          balance(x, xv, RedTree(y, yv, a, b), bl)
        case (RedTree(y, yv, a, BlackTree(z, zv, b, c)), bl) =>
          RedTree(z, zv, balance(y, yv, subl(a), b), BlackTree(x, xv, c, bl))
        case _ => sys.error("Defect: invariance violation at "+left)
      }
      def delLeft = left match {
        case _: BlackTree[_] => balLeft(key, value, left.del(k), right)
        case _ => RedTree(key, value, left.del(k), right)
      }
      def delRight = right match {
        case _: BlackTree[_] => balRight(key, value, left, right.del(k))
        case _ => RedTree(key, value, left, right.del(k))
      }
      def append(tl: Tree[B], tr: Tree[B]): Tree[B] = (tl, tr) match {
        case (Empty, t) => t
        case (t, Empty) => t
        case (RedTree(x, xv, a, b), RedTree(y, yv, c, d)) =>
          append(b, c) match {
            case RedTree(z, zv, bb, cc) => RedTree(z, zv, RedTree(x, xv, a, bb), RedTree(y, yv, cc, d))
            case bc => RedTree(x, xv, a, RedTree(y, yv, bc, d))
          }
        case (BlackTree(x, xv, a, b), BlackTree(y, yv, c, d)) =>
          append(b, c) match {
            case RedTree(z, zv, bb, cc) => RedTree(z, zv, BlackTree(x, xv, a, bb), BlackTree(y, yv, cc, d))
            case bc => balLeft(x, xv, a, BlackTree(y, yv, bc, d))
          }
        case (a, RedTree(x, xv, b, c)) => RedTree(x, xv, append(a, b), c)
        case (RedTree(x, xv, a, b), c) => RedTree(x, xv, a, append(b, c))
      }
      // RedBlack is neither A : Ordering[A], nor A <% Ordered[A]
      k match {
        case _ if isSmaller(k, key) => delLeft
        case _ if isSmaller(key, k) => delRight
        case _ => append(left, right)
      }
    }

    def smallest: NonEmpty[B] = if (left.isEmpty) this else left.smallest

    def toStream: Stream[(A,B)] =
      left.toStream ++ Stream((key,value)) ++ right.toStream

    def iterator: Iterator[(A, B)] =
      left.iterator ++ Iterator.single(Pair(key, value)) ++ right.iterator

    def foreach[U](f: (A, B) => U) {
      left foreach f
      f(key, value)
      right foreach f
    }

    override def rng(from: Option[A], until: Option[A]): Tree[B] = {
      if (from == None && until == None) return this
      if (from != None && isSmaller(key, from.get)) return right.rng(from, until);
      if (until != None && (isSmaller(until.get,key) || !isSmaller(key,until.get)))
        return left.rng(from, until);
      val newLeft = left.rng(from, None)
      val newRight = right.rng(None, until)
      if ((newLeft eq left) && (newRight eq right)) this
      else if (newLeft eq Empty) newRight.upd(key, value);
      else if (newRight eq Empty) newLeft.upd(key, value);
      else rebalance(newLeft, newRight)
    }

    // The zipper returned might have been traversed left-most (always the left child)
    // or right-most (always the right child). Left trees are traversed right-most,
    // and right trees are traversed leftmost.

    // Returns the zipper for the side with deepest black nodes depth, a flag
    // indicating whether the trees were unbalanced at all, and a flag indicating
    // whether the zipper was traversed left-most or right-most.

    // If the trees were balanced, returns an empty zipper
    private[this] def compareDepth(left: Tree[B], right: Tree[B]): (List[NonEmpty[B]], Boolean, Boolean, Int) = {
      // Once a side is found to be deeper, unzip it to the bottom
      def unzip(zipper: List[NonEmpty[B]], leftMost: Boolean): List[NonEmpty[B]] = {
        val next = if (leftMost) zipper.head.left else zipper.head.right
        next match {
          case node: NonEmpty[_] => unzip(node :: zipper, leftMost)
          case Empty             => zipper
        }
      }

      // Unzip left tree on the rightmost side and right tree on the leftmost side until one is
      // found to be deeper, or the bottom is reached
      def unzipBoth(left: Tree[B],
                    right: Tree[B],
                    leftZipper: List[NonEmpty[B]],
                    rightZipper: List[NonEmpty[B]],
                    smallerDepth: Int): (List[NonEmpty[B]], Boolean, Boolean, Int) = (left, right) match {
        case (l @ BlackTree(_, _, _, _), r @ BlackTree(_, _, _, _)) =>
          unzipBoth(l.right, r.left, l :: leftZipper, r :: rightZipper, smallerDepth + 1)
        case (l @ RedTree(_, _, _, _), r @ RedTree(_, _, _, _)) =>
          unzipBoth(l.right, r.left, l :: leftZipper, r :: rightZipper, smallerDepth)
        case (_, r @ RedTree(_, _, _, _)) =>
          unzipBoth(left, r.left, leftZipper, r :: rightZipper, smallerDepth)
        case (l @ RedTree(_, _, _, _), _) =>
          unzipBoth(l.right, right, l :: leftZipper, rightZipper, smallerDepth)
        case (Empty, Empty) =>
          (Nil, true, false, smallerDepth)
        case (Empty, r @ BlackTree(_, _, _, _)) =>
          val leftMost = true
          (unzip(r :: rightZipper, leftMost), false, leftMost, smallerDepth)
        case (l @ BlackTree(_, _, _, _), Empty) =>
          val leftMost = false
          (unzip(l :: leftZipper, leftMost), false, leftMost, smallerDepth)
      }
      unzipBoth(left, right, Nil, Nil, 0)
    }

    private[this] def rebalance(newLeft: Tree[B], newRight: Tree[B]) = {
      // This is like drop(n-1), but only counting black nodes
      def  findDepth(zipper: List[NonEmpty[B]], depth: Int): List[NonEmpty[B]] = zipper match {
        case BlackTree(_, _, _, _) :: tail =>
          if (depth == 1) zipper else findDepth(tail, depth - 1)
        case _ :: tail => findDepth(tail, depth)
        case Nil => sys.error("Defect: unexpected empty zipper while computing range")
      }

      // Blackening the smaller tree avoids balancing problems on union;
      // this can't be done later, though, or it would change the result of compareDepth
      val blkNewLeft = blacken(newLeft)
      val blkNewRight = blacken(newRight)
      val (zipper, levelled, leftMost, smallerDepth) = compareDepth(blkNewLeft, blkNewRight)

      if (levelled) {
        BlackTree(key, value, blkNewLeft, blkNewRight)
      } else {
        val zipFrom = findDepth(zipper, smallerDepth)
        val union = if (leftMost) {
          RedTree(key, value, blkNewLeft, zipFrom.head)
        } else {
          RedTree(key, value, zipFrom.head, blkNewRight)
        }
        val zippedTree = zipFrom.tail.foldLeft(union: Tree[B]) { (tree, node) =>
            if (leftMost)
              balanceLeft(node.isBlack, node.key, node.value, tree, node.right)
            else
              balanceRight(node.isBlack, node.key, node.value, node.left, tree)
        }
        zippedTree
      }
    }
    def first = if (left .isEmpty) key else left.first
    def last  = if (right.isEmpty) key else right.last
    def count = 1 + left.count + right.count
  }
  case object Empty extends Tree[Nothing] {
    def isEmpty = true
    def isBlack = true
    def lookup(k: A): Tree[Nothing] = this
    def upd[B](k: A, v: B): Tree[B] = RedTree(k, v, Empty, Empty)
    def del(k: A): Tree[Nothing] = this
    def smallest: NonEmpty[Nothing] = throw new NoSuchElementException("empty map")
    def iterator: Iterator[(A, Nothing)] = Iterator.empty
    def toStream: Stream[(A,Nothing)] = Stream.empty

    def foreach[U](f: (A, Nothing) => U) {}

    def rng(from: Option[A], until: Option[A]) = this
    def first = throw new NoSuchElementException("empty map")
    def last = throw new NoSuchElementException("empty map")
    def count = 0
  }
  case class RedTree[+B](override val key: A,
                         override val value: B,
                         override val left: Tree[B],
                         override val right: Tree[B]) extends NonEmpty[B] {
    def isBlack = false
  }
  case class BlackTree[+B](override val key: A,
                           override val value: B,
                           override val left: Tree[B],
                           override val right: Tree[B]) extends NonEmpty[B] {
    def isBlack = true
  }
}
