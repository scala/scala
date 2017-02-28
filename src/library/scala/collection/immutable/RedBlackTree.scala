/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package immutable

import scala.annotation.tailrec
import scala.annotation.meta.getter

/** An object containing the RedBlack tree implementation used by for `TreeMaps` and `TreeSets`.
 *
 *  Implementation note: since efficiency is important for data structures this implementation
 *  uses `null` to represent empty trees. This also means pattern matching cannot
 *  easily be used. The API represented by the RedBlackTree object tries to hide these
 *  optimizations behind a reasonably clean API.
 *
 *  @since 2.10
 */
private[collection]
object RedBlackTree {

  def isEmpty(tree: Tree[_, _]): Boolean = tree eq null

  def contains[A: Ordering](tree: Tree[A, _], x: A): Boolean = lookup(tree, x) ne null
  def get[A: Ordering, B](tree: Tree[A, B], x: A): Option[B] = lookup(tree, x) match {
    case null => None
    case tree => Some(tree.value)
  }

  @tailrec
  def lookup[A, B](tree: Tree[A, B], x: A)(implicit ordering: Ordering[A]): Tree[A, B] = if (tree eq null) null else {
    val cmp = ordering.compare(x, tree.key)
    if (cmp < 0) lookup(tree.left, x)
    else if (cmp > 0) lookup(tree.right, x)
    else tree
  }

  def count(tree: Tree[_, _]) = if (tree eq null) 0 else tree.count
  /**
   * Count all the nodes with keys greater than or equal to the lower bound and less than the upper bound.
   * The two bounds are optional.
   */
  def countInRange[A](tree: Tree[A, _], from: Option[A], to:Option[A])(implicit ordering: Ordering[A]) : Int =
    if (tree eq null) 0 else
    (from, to) match {
      // with no bounds use this node's count
      case (None, None) => tree.count
      // if node is less than the lower bound, try the tree on the right, it might be in range
      case (Some(lb), _) if ordering.lt(tree.key, lb) => countInRange(tree.right, from, to)
      // if node is greater than or equal to the upper bound, try the tree on the left, it might be in range
      case (_, Some(ub)) if ordering.gteq(tree.key, ub) => countInRange(tree.left, from, to)
      // node is in range so the tree on the left will all be less than the upper bound and the tree on the
      // right will all be greater than or equal to the lower bound. So 1 for this node plus
      // count the subtrees by stripping off the bounds that we don't need any more
      case _ => 1 + countInRange(tree.left, from, None) + countInRange(tree.right, None, to)

    }
  def update[A: Ordering, B, B1 >: B](tree: Tree[A, B], k: A, v: B1, overwrite: Boolean): Tree[A, B1] = blacken(upd(tree, k, v, overwrite))
  def delete[A: Ordering, B](tree: Tree[A, B], k: A): Tree[A, B] = blacken(del(tree, k))
  def rangeImpl[A: Ordering, B](tree: Tree[A, B], from: Option[A], until: Option[A]): Tree[A, B] = (from, until) match {
    case (Some(from), Some(until)) => this.range(tree, from, until)
    case (Some(from), None)        => this.from(tree, from)
    case (None,       Some(until)) => this.until(tree, until)
    case (None,       None)        => tree
  }
  def range[A: Ordering, B](tree: Tree[A, B], from: A, until: A): Tree[A, B] = blacken(doRange(tree, from, until))
  def from[A: Ordering, B](tree: Tree[A, B], from: A): Tree[A, B] = blacken(doFrom(tree, from))
  def to[A: Ordering, B](tree: Tree[A, B], to: A): Tree[A, B] = blacken(doTo(tree, to))
  def until[A: Ordering, B](tree: Tree[A, B], key: A): Tree[A, B] = blacken(doUntil(tree, key))

  def drop[A: Ordering, B](tree: Tree[A, B], n: Int): Tree[A, B] = blacken(doDrop(tree, n))
  def take[A: Ordering, B](tree: Tree[A, B], n: Int): Tree[A, B] = blacken(doTake(tree, n))
  def slice[A: Ordering, B](tree: Tree[A, B], from: Int, until: Int): Tree[A, B] = blacken(doSlice(tree, from, until))

  def smallest[A, B](tree: Tree[A, B]): Tree[A, B] = {
    if (tree eq null) throw new NoSuchElementException("empty map")
    var result = tree
    while (result.left ne null) result = result.left
    result
  }
  def greatest[A, B](tree: Tree[A, B]): Tree[A, B] = {
    if (tree eq null) throw new NoSuchElementException("empty map")
    var result = tree
    while (result.right ne null) result = result.right
    result
  }


  def foreach[A,B,U](tree:Tree[A,B], f:((A,B)) => U):Unit = if (tree ne null) _foreach(tree,f)

  private[this] def _foreach[A, B, U](tree: Tree[A, B], f: ((A, B)) => U) {
    if (tree.left ne null) _foreach(tree.left, f)
    f((tree.key, tree.value))
    if (tree.right ne null) _foreach(tree.right, f)
  }

  def foreachKey[A, U](tree:Tree[A,_], f: A => U):Unit = if (tree ne null) _foreachKey(tree,f)

  private[this] def _foreachKey[A, U](tree: Tree[A, _], f: A => U) {
    if (tree.left ne null) _foreachKey(tree.left, f)
    f((tree.key))
    if (tree.right ne null) _foreachKey(tree.right, f)
  }

  def iterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None): Iterator[(A, B)] = new EntriesIterator(tree, start)
  def keysIterator[A: Ordering](tree: Tree[A, _], start: Option[A] = None): Iterator[A] = new KeysIterator(tree, start)
  def valuesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None): Iterator[B] = new ValuesIterator(tree, start)

  @tailrec
  def nth[A, B](tree: Tree[A, B], n: Int): Tree[A, B] = {
    val count = this.count(tree.left)
    if (n < count) nth(tree.left, n)
    else if (n > count) nth(tree.right, n - count - 1)
    else tree
  }

  def isBlack(tree: Tree[_, _]) = (tree eq null) || isBlackTree(tree)

  private[this] def isRedTree(tree: Tree[_, _]) = tree.isInstanceOf[RedTree[_, _]]
  private[this] def isBlackTree(tree: Tree[_, _]) = tree.isInstanceOf[BlackTree[_, _]]

  private[this] def blacken[A, B](t: Tree[A, B]): Tree[A, B] = if (t eq null) null else t.black

  private[this] def mkTree[A, B](isBlack: Boolean, k: A, v: B, l: Tree[A, B], r: Tree[A, B]) =
    if (isBlack) BlackTree(k, v, l, r) else RedTree(k, v, l, r)

  private[this] def balanceLeft[A, B, B1 >: B](isBlack: Boolean, z: A, zv: B, l: Tree[A, B1], d: Tree[A, B1]): Tree[A, B1] = {
    if (isRedTree(l) && isRedTree(l.left))
      RedTree(l.key, l.value, BlackTree(l.left.key, l.left.value, l.left.left, l.left.right), BlackTree(z, zv, l.right, d))
    else if (isRedTree(l) && isRedTree(l.right))
      RedTree(l.right.key, l.right.value, BlackTree(l.key, l.value, l.left, l.right.left), BlackTree(z, zv, l.right.right, d))
    else
      mkTree(isBlack, z, zv, l, d)
  }
  private[this] def balanceRight[A, B, B1 >: B](isBlack: Boolean, x: A, xv: B, a: Tree[A, B1], r: Tree[A, B1]): Tree[A, B1] = {
    if (isRedTree(r) && isRedTree(r.left))
      RedTree(r.left.key, r.left.value, BlackTree(x, xv, a, r.left.left), BlackTree(r.key, r.value, r.left.right, r.right))
    else if (isRedTree(r) && isRedTree(r.right))
      RedTree(r.key, r.value, BlackTree(x, xv, a, r.left), BlackTree(r.right.key, r.right.value, r.right.left, r.right.right))
    else
      mkTree(isBlack, x, xv, a, r)
  }
  private[this] def upd[A, B, B1 >: B](tree: Tree[A, B], k: A, v: B1, overwrite: Boolean)(implicit ordering: Ordering[A]): Tree[A, B1] = if (tree eq null) {
    RedTree(k, v, null, null)
  } else {
    val cmp = ordering.compare(k, tree.key)
    if (cmp < 0) balanceLeft(isBlackTree(tree), tree.key, tree.value, upd(tree.left, k, v, overwrite), tree.right)
    else if (cmp > 0) balanceRight(isBlackTree(tree), tree.key, tree.value, tree.left, upd(tree.right, k, v, overwrite))
    else if (overwrite || k != tree.key) mkTree(isBlackTree(tree), k, v, tree.left, tree.right)
    else tree
  }
  private[this] def updNth[A, B, B1 >: B](tree: Tree[A, B], idx: Int, k: A, v: B1, overwrite: Boolean): Tree[A, B1] = if (tree eq null) {
    RedTree(k, v, null, null)
  } else {
    val rank = count(tree.left) + 1
    if (idx < rank) balanceLeft(isBlackTree(tree), tree.key, tree.value, updNth(tree.left, idx, k, v, overwrite), tree.right)
    else if (idx > rank) balanceRight(isBlackTree(tree), tree.key, tree.value, tree.left, updNth(tree.right, idx - rank, k, v, overwrite))
    else if (overwrite) mkTree(isBlackTree(tree), k, v, tree.left, tree.right)
    else tree
  }

  /* Based on Stefan Kahrs' Haskell version of Okasaki's Red&Black Trees
   * Constructing Red-Black Trees, Ralf Hinze: http://www.cs.ox.ac.uk/ralf.hinze/publications/WAAAPL99b.ps.gz
   * Red-Black Trees in a Functional Setting, Chris Okasaki: https://wiki.rice.edu/confluence/download/attachments/2761212/Okasaki-Red-Black.pdf */
  private[this] def del[A, B](tree: Tree[A, B], k: A)(implicit ordering: Ordering[A]): Tree[A, B] = if (tree eq null) null else {
    def balance(x: A, xv: B, tl: Tree[A, B], tr: Tree[A, B]) = if (isRedTree(tl)) {
      if (isRedTree(tr)) {
        RedTree(x, xv, tl.black, tr.black)
      } else if (isRedTree(tl.left)) {
        RedTree(tl.key, tl.value, tl.left.black, BlackTree(x, xv, tl.right, tr))
      } else if (isRedTree(tl.right)) {
        RedTree(tl.right.key, tl.right.value, BlackTree(tl.key, tl.value, tl.left, tl.right.left), BlackTree(x, xv, tl.right.right, tr))
      } else {
        BlackTree(x, xv, tl, tr)
      }
    } else if (isRedTree(tr)) {
      if (isRedTree(tr.right)) {
        RedTree(tr.key, tr.value, BlackTree(x, xv, tl, tr.left), tr.right.black)
      } else if (isRedTree(tr.left)) {
        RedTree(tr.left.key, tr.left.value, BlackTree(x, xv, tl, tr.left.left), BlackTree(tr.key, tr.value, tr.left.right, tr.right))
      } else {
        BlackTree(x, xv, tl, tr)
      }
    } else {
      BlackTree(x, xv, tl, tr)
    }
    def subl(t: Tree[A, B]) =
      if (t.isInstanceOf[BlackTree[_, _]]) t.red
      else sys.error("Defect: invariance violation; expected black, got "+t)

    def balLeft(x: A, xv: B, tl: Tree[A, B], tr: Tree[A, B]) = if (isRedTree(tl)) {
      RedTree(x, xv, tl.black, tr)
    } else if (isBlackTree(tr)) {
      balance(x, xv, tl, tr.red)
    } else if (isRedTree(tr) && isBlackTree(tr.left)) {
      RedTree(tr.left.key, tr.left.value, BlackTree(x, xv, tl, tr.left.left), balance(tr.key, tr.value, tr.left.right, subl(tr.right)))
    } else {
      sys.error("Defect: invariance violation")
    }
    def balRight(x: A, xv: B, tl: Tree[A, B], tr: Tree[A, B]) = if (isRedTree(tr)) {
      RedTree(x, xv, tl, tr.black)
    } else if (isBlackTree(tl)) {
      balance(x, xv, tl.red, tr)
    } else if (isRedTree(tl) && isBlackTree(tl.right)) {
      RedTree(tl.right.key, tl.right.value, balance(tl.key, tl.value, subl(tl.left), tl.right.left), BlackTree(x, xv, tl.right.right, tr))
    } else {
      sys.error("Defect: invariance violation")
    }
    def delLeft = if (isBlackTree(tree.left)) balLeft(tree.key, tree.value, del(tree.left, k), tree.right) else RedTree(tree.key, tree.value, del(tree.left, k), tree.right)
    def delRight = if (isBlackTree(tree.right)) balRight(tree.key, tree.value, tree.left, del(tree.right, k)) else RedTree(tree.key, tree.value, tree.left, del(tree.right, k))
    def append(tl: Tree[A, B], tr: Tree[A, B]): Tree[A, B] = if (tl eq null) {
      tr
    } else if (tr eq null) {
      tl
    } else if (isRedTree(tl) && isRedTree(tr)) {
      val bc = append(tl.right, tr.left)
      if (isRedTree(bc)) {
        RedTree(bc.key, bc.value, RedTree(tl.key, tl.value, tl.left, bc.left), RedTree(tr.key, tr.value, bc.right, tr.right))
      } else {
        RedTree(tl.key, tl.value, tl.left, RedTree(tr.key, tr.value, bc, tr.right))
      }
    } else if (isBlackTree(tl) && isBlackTree(tr)) {
      val bc = append(tl.right, tr.left)
      if (isRedTree(bc)) {
        RedTree(bc.key, bc.value, BlackTree(tl.key, tl.value, tl.left, bc.left), BlackTree(tr.key, tr.value, bc.right, tr.right))
      } else {
        balLeft(tl.key, tl.value, tl.left, BlackTree(tr.key, tr.value, bc, tr.right))
      }
    } else if (isRedTree(tr)) {
      RedTree(tr.key, tr.value, append(tl, tr.left), tr.right)
    } else if (isRedTree(tl)) {
      RedTree(tl.key, tl.value, tl.left, append(tl.right, tr))
    } else {
      sys.error("unmatched tree on append: " + tl + ", " + tr)
    }

    val cmp = ordering.compare(k, tree.key)
    if (cmp < 0) delLeft
    else if (cmp > 0) delRight
    else append(tree.left, tree.right)
  }

  private[this] def doFrom[A, B](tree: Tree[A, B], from: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
    if (tree eq null) return null
    if (ordering.lt(tree.key, from)) return doFrom(tree.right, from)
    val newLeft = doFrom(tree.left, from)
    if (newLeft eq tree.left) tree
    else if (newLeft eq null) upd(tree.right, tree.key, tree.value, overwrite = false)
    else rebalance(tree, newLeft, tree.right)
  }
  private[this] def doTo[A, B](tree: Tree[A, B], to: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
    if (tree eq null) return null
    if (ordering.lt(to, tree.key)) return doTo(tree.left, to)
    val newRight = doTo(tree.right, to)
    if (newRight eq tree.right) tree
    else if (newRight eq null) upd(tree.left, tree.key, tree.value, overwrite = false)
    else rebalance(tree, tree.left, newRight)
  }
  private[this] def doUntil[A, B](tree: Tree[A, B], until: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
    if (tree eq null) return null
    if (ordering.lteq(until, tree.key)) return doUntil(tree.left, until)
    val newRight = doUntil(tree.right, until)
    if (newRight eq tree.right) tree
    else if (newRight eq null) upd(tree.left, tree.key, tree.value, overwrite = false)
    else rebalance(tree, tree.left, newRight)
  }
  private[this] def doRange[A, B](tree: Tree[A, B], from: A, until: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
    if (tree eq null) return null
    if (ordering.lt(tree.key, from)) return doRange(tree.right, from, until)
    if (ordering.lteq(until, tree.key)) return doRange(tree.left, from, until)
    val newLeft = doFrom(tree.left, from)
    val newRight = doUntil(tree.right, until)
    if ((newLeft eq tree.left) && (newRight eq tree.right)) tree
    else if (newLeft eq null) upd(newRight, tree.key, tree.value, overwrite = false)
    else if (newRight eq null) upd(newLeft, tree.key, tree.value, overwrite = false)
    else rebalance(tree, newLeft, newRight)
  }

  private[this] def doDrop[A, B](tree: Tree[A, B], n: Int): Tree[A, B] = {
    if (n <= 0) return tree
    if (n >= this.count(tree)) return null
    val count = this.count(tree.left)
    if (n > count) return doDrop(tree.right, n - count - 1)
    val newLeft = doDrop(tree.left, n)
    if (newLeft eq tree.left) tree
    else if (newLeft eq null) updNth(tree.right, n - count - 1, tree.key, tree.value, overwrite = false)
    else rebalance(tree, newLeft, tree.right)
  }
  private[this] def doTake[A, B](tree: Tree[A, B], n: Int): Tree[A, B] = {
    if (n <= 0) return null
    if (n >= this.count(tree)) return tree
    val count = this.count(tree.left)
    if (n <= count) return doTake(tree.left, n)
    val newRight = doTake(tree.right, n - count - 1)
    if (newRight eq tree.right) tree
    else if (newRight eq null) updNth(tree.left, n, tree.key, tree.value, overwrite = false)
    else rebalance(tree, tree.left, newRight)
  }
  private[this] def doSlice[A, B](tree: Tree[A, B], from: Int, until: Int): Tree[A, B] = {
    if (tree eq null) return null
    val count = this.count(tree.left)
    if (from > count) return doSlice(tree.right, from - count - 1, until - count - 1)
    if (until <= count) return doSlice(tree.left, from, until)
    val newLeft = doDrop(tree.left, from)
    val newRight = doTake(tree.right, until - count - 1)
    if ((newLeft eq tree.left) && (newRight eq tree.right)) tree
    else if (newLeft eq null) updNth(newRight, from - count - 1, tree.key, tree.value, overwrite = false)
    else if (newRight eq null) updNth(newLeft, until, tree.key, tree.value, overwrite = false)
    else rebalance(tree, newLeft, newRight)
  }

  // The zipper returned might have been traversed left-most (always the left child)
  // or right-most (always the right child). Left trees are traversed right-most,
  // and right trees are traversed leftmost.

  // Returns the zipper for the side with deepest black nodes depth, a flag
  // indicating whether the trees were unbalanced at all, and a flag indicating
  // whether the zipper was traversed left-most or right-most.

  // If the trees were balanced, returns an empty zipper
  private[this] def compareDepth[A, B](left: Tree[A, B], right: Tree[A, B]): (NList[Tree[A, B]], Boolean, Boolean, Int) = {
    import NList.cons
    // Once a side is found to be deeper, unzip it to the bottom
    def unzip(zipper: NList[Tree[A, B]], leftMost: Boolean): NList[Tree[A, B]] = {
      val next = if (leftMost) zipper.head.left else zipper.head.right
      if (next eq null) zipper
      else unzip(cons(next, zipper), leftMost)
    }

    // Unzip left tree on the rightmost side and right tree on the leftmost side until one is
    // found to be deeper, or the bottom is reached
    def unzipBoth(left: Tree[A, B],
                  right: Tree[A, B],
                  leftZipper: NList[Tree[A, B]],
                  rightZipper: NList[Tree[A, B]],
                  smallerDepth: Int): (NList[Tree[A, B]], Boolean, Boolean, Int) = {
      if (isBlackTree(left) && isBlackTree(right)) {
        unzipBoth(left.right, right.left, cons(left, leftZipper), cons(right, rightZipper), smallerDepth + 1)
      } else if (isRedTree(left) && isRedTree(right)) {
        unzipBoth(left.right, right.left, cons(left, leftZipper), cons(right, rightZipper), smallerDepth)
      } else if (isRedTree(right)) {
        unzipBoth(left, right.left, leftZipper, cons(right, rightZipper), smallerDepth)
      } else if (isRedTree(left)) {
        unzipBoth(left.right, right, cons(left, leftZipper), rightZipper, smallerDepth)
      } else if ((left eq null) && (right eq null)) {
        (null, true, false, smallerDepth)
      } else if ((left eq null) && isBlackTree(right)) {
        val leftMost = true
        (unzip(cons(right, rightZipper), leftMost), false, leftMost, smallerDepth)
      } else if (isBlackTree(left) && (right eq null)) {
        val leftMost = false
        (unzip(cons(left, leftZipper), leftMost), false, leftMost, smallerDepth)
      } else {
        sys.error("unmatched trees in unzip: " + left + ", " + right)
      }
    }
    unzipBoth(left, right, null, null, 0)
  }

  private[this] def rebalance[A, B](tree: Tree[A, B], newLeft: Tree[A, B], newRight: Tree[A, B]) = {
    // This is like drop(n-1), but only counting black nodes
    @tailrec
    def  findDepth(zipper: NList[Tree[A, B]], depth: Int): NList[Tree[A, B]] =
      if (zipper eq null) {
        sys.error("Defect: unexpected empty zipper while computing range")
      } else if (isBlackTree(zipper.head)) {
        if (depth == 1) zipper else findDepth(zipper.tail, depth - 1)
      } else {
        findDepth(zipper.tail, depth)
      }

    // Blackening the smaller tree avoids balancing problems on union;
    // this can't be done later, though, or it would change the result of compareDepth
    val blkNewLeft = blacken(newLeft)
    val blkNewRight = blacken(newRight)
    val (zipper, levelled, leftMost, smallerDepth) = compareDepth(blkNewLeft, blkNewRight)

    if (levelled) {
      BlackTree(tree.key, tree.value, blkNewLeft, blkNewRight)
    } else {
      val zipFrom = findDepth(zipper, smallerDepth)
      val union = if (leftMost) {
        RedTree(tree.key, tree.value, blkNewLeft, zipFrom.head)
      } else {
        RedTree(tree.key, tree.value, zipFrom.head, blkNewRight)
      }
      val zippedTree = NList.foldLeft(zipFrom.tail, union: Tree[A, B]) { (tree, node) =>
        if (leftMost)
          balanceLeft(isBlackTree(node), node.key, node.value, tree, node.right)
        else
          balanceRight(isBlackTree(node), node.key, node.value, node.left, tree)
      }
      zippedTree
    }
  }

  // Null optimized list implementation for tree rebalancing. null presents Nil.
  private[this] final class NList[A](val head: A, val tail: NList[A])

  private[this] final object NList {

    def cons[B](x: B, xs: NList[B]): NList[B] = new NList(x, xs)

    def foldLeft[A, B](xs: NList[A], z: B)(op: (B, A) => B): B = {
      var acc = z
      var these = xs
      while (these ne null) {
        acc = op(acc, these.head)
        these = these.tail
      }
      acc
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
  sealed abstract class Tree[A, +B](
    @(inline @getter) final val key: A,
    @(inline @getter) final val value: B,
    @(inline @getter) final val left: Tree[A, B],
    @(inline @getter) final val right: Tree[A, B])
  extends Serializable {
    @(inline @getter) final val count: Int = 1 + RedBlackTree.count(left) + RedBlackTree.count(right)
    def black: Tree[A, B]
    def red: Tree[A, B]
  }
  final class RedTree[A, +B](key: A,
                             value: B,
                             left: Tree[A, B],
                             right: Tree[A, B]) extends Tree[A, B](key, value, left, right) {
    override def black: Tree[A, B] = BlackTree(key, value, left, right)
    override def red: Tree[A, B] = this
    override def toString: String = "RedTree(" + key + ", " + value + ", " + left + ", " + right + ")"
  }
  final class BlackTree[A, +B](key: A,
                               value: B,
                               left: Tree[A, B],
                               right: Tree[A, B]) extends Tree[A, B](key, value, left, right) {
    override def black: Tree[A, B] = this
    override def red: Tree[A, B] = RedTree(key, value, left, right)
    override def toString: String = "BlackTree(" + key + ", " + value + ", " + left + ", " + right + ")"
  }

  object RedTree {
    @inline def apply[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = new RedTree(key, value, left, right)
    def unapply[A, B](t: RedTree[A, B]) = Some((t.key, t.value, t.left, t.right))
  }
  object BlackTree {
    @inline def apply[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = new BlackTree(key, value, left, right)
    def unapply[A, B](t: BlackTree[A, B]) = Some((t.key, t.value, t.left, t.right))
  }

  private[this] abstract class TreeIterator[A, B, R](root: Tree[A, B], start: Option[A])(implicit ordering: Ordering[A]) extends Iterator[R] {
    protected[this] def nextResult(tree: Tree[A, B]): R

    override def hasNext: Boolean = lookahead ne null

    override def next: R = lookahead match {
      case null =>
        throw new NoSuchElementException("next on empty iterator")
      case tree =>
        lookahead = findLeftMostOrPopOnEmpty(goRight(tree))
        nextResult(tree)
    }

    @tailrec
    private[this] def findLeftMostOrPopOnEmpty(tree: Tree[A, B]): Tree[A, B] =
      if (tree eq null) popNext()
      else if (tree.left eq null) tree
      else findLeftMostOrPopOnEmpty(goLeft(tree))

    private[this] def pushNext(tree: Tree[A, B]) {
      try {
        stackOfNexts(index) = tree
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
          assert(index >= stackOfNexts.length)
          stackOfNexts :+= null
          pushNext(tree)
      }
    }
    private[this] def popNext(): Tree[A, B] = if (index == 0) null else {
      index -= 1
      stackOfNexts(index)
    }

    private[this] var stackOfNexts = if (root eq null) null else {
      /*
       * According to "Ralf Hinze. Constructing red-black trees" [http://www.cs.ox.ac.uk/ralf.hinze/publications/#P5]
       * the maximum height of a red-black tree is 2*log_2(n + 2) - 2.
       *
       * According to {@see Integer#numberOfLeadingZeros} ceil(log_2(n)) = (32 - Integer.numberOfLeadingZeros(n - 1))
       *
       * Although we don't store the deepest nodes in the path during iteration,
       * we potentially do so in `startFrom`.
       */
      val maximumHeight = 2 * (32 - Integer.numberOfLeadingZeros(root.count + 2 - 1)) - 2
      new Array[Tree[A, B]](maximumHeight)
    }
    private[this] var index = 0
    private[this] var lookahead: Tree[A, B] = start map startFrom getOrElse findLeftMostOrPopOnEmpty(root)

    /**
     * Find the leftmost subtree whose key is equal to the given key, or if no such thing,
     * the leftmost subtree with the key that would be "next" after it according
     * to the ordering. Along the way build up the iterator's path stack so that "next"
     * functionality works.
     */
    private[this] def startFrom(key: A) : Tree[A,B] = if (root eq null) null else {
      @tailrec def find(tree: Tree[A, B]): Tree[A, B] =
        if (tree eq null) popNext()
        else find(
          if (ordering.lteq(key, tree.key)) goLeft(tree)
          else goRight(tree)
        )
      find(root)
    }

    private[this] def goLeft(tree: Tree[A, B]) = {
      pushNext(tree)
      tree.left
    }

    private[this] def goRight(tree: Tree[A, B]) = tree.right
  }

  private[this] class EntriesIterator[A: Ordering, B](tree: Tree[A, B], focus: Option[A]) extends TreeIterator[A, B, (A, B)](tree, focus) {
    override def nextResult(tree: Tree[A, B]) = (tree.key, tree.value)
  }

  private[this] class KeysIterator[A: Ordering, B](tree: Tree[A, B], focus: Option[A]) extends TreeIterator[A, B, A](tree, focus) {
    override def nextResult(tree: Tree[A, B]) = tree.key
  }

  private[this] class ValuesIterator[A: Ordering, B](tree: Tree[A, B], focus: Option[A]) extends TreeIterator[A, B, B](tree, focus) {
    override def nextResult(tree: Tree[A, B]) = tree.value
  }
}
