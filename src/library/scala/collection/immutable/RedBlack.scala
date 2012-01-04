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
 *  @since 2.3
 */
private[immutable]
object RedBlack {

  private def blacken[A, B](t: Tree[A, B]): Tree[A, B] = if (t eq null) null else t.black

  private def mkTree[A, B](isBlack: Boolean, k: A, v: B, l: Tree[A, B], r: Tree[A, B]) =
    if (isBlack) BlackTree(k, v, l, r) else RedTree(k, v, l, r)

  def isBlack(tree: Tree[_, _]) = (tree eq null) || isBlackTree(tree)
  def isRedTree(tree: Tree[_, _]) = tree.isInstanceOf[RedTree[_, _]]
  def isBlackTree(tree: Tree[_, _]) = tree.isInstanceOf[BlackTree[_, _]]

  def isEmpty(tree: Tree[_, _]): Boolean = tree eq null

  def contains[A](tree: Tree[A, _], x: A)(implicit ordering: Ordering[A]): Boolean = lookup(tree, x) ne null
  def get[A, B](tree: Tree[A, B], x: A)(implicit ordering: Ordering[A]): Option[B] = lookup(tree, x) match {
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
  def update[A, B, B1 >: B](tree: Tree[A, B], k: A, v: B1)(implicit ordering: Ordering[A]): Tree[A, B1] = blacken(upd(tree, k, v))
  def delete[A, B](tree: Tree[A, B], k: A)(implicit ordering: Ordering[A]): Tree[A, B] = blacken(del(tree, k))
  def range[A, B](tree: Tree[A, B], from: Option[A], until: Option[A])(implicit ordering: Ordering[A]): Tree[A, B] = blacken(rng(tree, from, until))

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

  def foreach[A, B, U](tree: Tree[A, B], f: ((A, B)) => U): Unit = if (tree ne null) {
    if (tree.left ne null) foreach(tree.left, f)
    f((tree.key, tree.value))
    if (tree.right ne null) foreach(tree.right, f)
  }
  def foreachKey[A, U](tree: Tree[A, _], f: A => U): Unit = if (tree ne null) {
    if (tree.left ne null) foreachKey(tree.left, f)
    f(tree.key)
    if (tree.right ne null) foreachKey(tree.right, f)
  }

  def iterator[A, B](tree: Tree[A, B]): Iterator[(A, B)] = new EntriesIterator(tree)
  def keysIterator[A, _](tree: Tree[A, _]): Iterator[A] = new KeysIterator(tree)
  def valuesIterator[_, B](tree: Tree[_, B]): Iterator[B] = new ValuesIterator(tree)

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
  private[this] def upd[A, B, B1 >: B](tree: Tree[A, B], k: A, v: B1)(implicit ordering: Ordering[A]): Tree[A, B1] = if (tree == null) {
    RedTree(k, v, null, null)
  } else {
    val cmp = ordering.compare(k, tree.key)
    if (cmp < 0) balanceLeft(tree.isBlack, tree.key, tree.value, upd(tree.left, k, v), tree.right)
    else if (cmp > 0) balanceRight(tree.isBlack, tree.key, tree.value, tree.left, upd(tree.right, k, v))
    else mkTree(tree.isBlack, k, v, tree.left, tree.right)
  }

    // Based on Stefan Kahrs' Haskell version of Okasaki's Red&Black Trees
    // http://www.cse.unsw.edu.au/~dons/data/RedBlackTree.html
  private[this] def del[A, B](tree: Tree[A, B], k: A)(implicit ordering: Ordering[A]): Tree[A, B] = if (tree == null) null else {
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
      sys.error("Defect: invariance violation at ") // TODO
    }
    def balRight(x: A, xv: B, tl: Tree[A, B], tr: Tree[A, B]) = if (isRedTree(tr)) {
      RedTree(x, xv, tl, tr.black)
    } else if (isBlackTree(tl)) {
      balance(x, xv, tl.red, tr)
    } else if (isRedTree(tl) && isBlackTree(tl.right)) {
      RedTree(tl.right.key, tl.right.value, balance(tl.key, tl.value, subl(tl.left), tl.right.left), BlackTree(x, xv, tl.right.right, tr))
    } else {
      sys.error("Defect: invariance violation at ") // TODO
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

  private[this] def rng[A, B](tree: Tree[A, B], from: Option[A], until: Option[A])(implicit ordering: Ordering[A]): Tree[A, B] = {
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
  private[this] def compareDepth[A, B](left: Tree[A, B], right: Tree[A, B]): (List[Tree[A, B]], Boolean, Boolean, Int) = {
    // Once a side is found to be deeper, unzip it to the bottom
    def unzip(zipper: List[Tree[A, B]], leftMost: Boolean): List[Tree[A, B]] = {
      val next = if (leftMost) zipper.head.left else zipper.head.right
      next match {
        case null => zipper
        case node => unzip(node :: zipper, leftMost)
      }
    }

    // Unzip left tree on the rightmost side and right tree on the leftmost side until one is
    // found to be deeper, or the bottom is reached
    def unzipBoth(left: Tree[A, B],
                  right: Tree[A, B],
                  leftZipper: List[Tree[A, B]],
                  rightZipper: List[Tree[A, B]],
                  smallerDepth: Int): (List[Tree[A, B]], Boolean, Boolean, Int) = {
      if (isBlackTree(left) && isBlackTree(right)) {
        unzipBoth(left.right, right.left, left :: leftZipper, right :: rightZipper, smallerDepth + 1)
      } else if (isRedTree(left) && isRedTree(right)) {
        unzipBoth(left.right, right.left, left :: leftZipper, right :: rightZipper, smallerDepth)
      } else if (isRedTree(right)) {
        unzipBoth(left, right.left, leftZipper, right :: rightZipper, smallerDepth)
      } else if (isRedTree(left)) {
        unzipBoth(left.right, right, left :: leftZipper, rightZipper, smallerDepth)
      } else if ((left eq null) && (right eq null)) {
        (Nil, true, false, smallerDepth)
      } else if ((left eq null) && isBlackTree(right)) {
        val leftMost = true
        (unzip(right :: rightZipper, leftMost), false, leftMost, smallerDepth)
      } else if (isBlackTree(left) && (right eq null)) {
        val leftMost = false
        (unzip(left :: leftZipper, leftMost), false, leftMost, smallerDepth)
      } else {
        sys.error("unmatched trees in unzip: " + left + ", " + right)
      }
    }
    unzipBoth(left, right, Nil, Nil, 0)
  }
   private[this] def rebalance[A, B](tree: Tree[A, B], newLeft: Tree[A, B], newRight: Tree[A, B]) = {
    // This is like drop(n-1), but only counting black nodes
    def  findDepth(zipper: List[Tree[A, B]], depth: Int): List[Tree[A, B]] = zipper match {
      case head :: tail if isBlackTree(head) =>
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
      BlackTree(tree.key, tree.value, blkNewLeft, blkNewRight)
    } else {
      val zipFrom = findDepth(zipper, smallerDepth)
      val union = if (leftMost) {
        RedTree(tree.key, tree.value, blkNewLeft, zipFrom.head)
      } else {
        RedTree(tree.key, tree.value, zipFrom.head, blkNewRight)
      }
      val zippedTree = zipFrom.tail.foldLeft(union: Tree[A, B]) { (tree, node) =>
        if (leftMost)
          balanceLeft(node.isBlack, node.key, node.value, tree, node.right)
        else
          balanceRight(node.isBlack, node.key, node.value, node.left, tree)
      }
      zippedTree
    }
  }

  sealed abstract class Tree[A, +B](
    @(inline @getter) final val key: A,
    @(inline @getter) final val value: B,
    @(inline @getter) final val left: Tree[A, B],
    @(inline @getter) final val right: Tree[A, B])
  extends Serializable {
    final val count: Int = 1 + RedBlack.count(left) + RedBlack.count(right)
    def isBlack: Boolean
    def nth(n: Int): Tree[A, B] = {
      val count = RedBlack.count(left)
      if (n < count) left.nth(n)
      else if (n > count) right.nth(n - count - 1)
      else this
    }
    def black: Tree[A, B]
    def red: Tree[A, B]
  }

  final class RedTree[A, +B](key: A,
                             value: B,
                             left: Tree[A, B],
                             right: Tree[A, B]) extends Tree[A, B](key, value, left, right) {
    override def isBlack = false
    override def black = BlackTree(key, value, left, right)
    override def red = this
    override def toString = "RedTree(" + key + ", " + value + ", " + left + ", " + right + ")"
  }
  object RedTree {
    def apply[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = new RedTree(key, value, left, right)
    def unapply[A, B](t: RedTree[A, B]) = Some((t.key, t.value, t.left, t.right))
  }
  final class BlackTree[A, +B](key: A,
                               value: B,
                               left: Tree[A, B],
                               right: Tree[A, B]) extends Tree[A, B](key, value, left, right) {
    override def isBlack = true
    override def black = this
    override def red = RedTree(key, value, left, right)
    override def toString = "BlackTree(" + key + ", " + value + ", " + left + ", " + right + ")"
  }
  object BlackTree {
    def apply[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = new BlackTree(key, value, left, right)
    def unapply[A, B](t: BlackTree[A, B]) = Some((t.key, t.value, t.left, t.right))
  }

  private[this] abstract class TreeIterator[A, B, R](tree: Tree[A, B]) extends Iterator[R] {
    protected[this] def nextResult(tree: Tree[A, B]): R

    override def hasNext: Boolean = next ne null

    override def next: R = next match {
      case null =>
        throw new NoSuchElementException("next on empty iterator")
      case tree =>
        next = findNext(tree.right)
        nextResult(tree)
    }

    @tailrec
    private[this] def findNext(tree: Tree[A, B]): Tree[A, B] = {
      if (tree eq null) popPath()
      else if (tree.left eq null) tree
      else {
        pushPath(tree)
        findNext(tree.left)
      }
    }

    private[this] def pushPath(tree: Tree[A, B]) {
      try {
        path(index) = tree
        index += 1
      } catch {
        case _: ArrayIndexOutOfBoundsException =>
          // Either the tree became unbalanced or we calculated the maximum height incorrectly.
          // To avoid crashing the iterator we expand the path array. Obviously this should never
          // happen...
          //
          // An exception handler is used instead of an if-condition to optimize the normal path.
          assert(index >= path.length)
          path :+= null
          pushPath(tree)
      }
    }
    private[this] def popPath(): Tree[A, B] = if (index == 0) null else {
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
      new Array[Tree[A, B]](maximumHeight)
    }
    private[this] var index = 0
    private[this] var next: Tree[A, B] = findNext(tree)
  }

  private[this] class EntriesIterator[A, B](tree: Tree[A, B]) extends TreeIterator[A, B, (A, B)](tree) {
    override def nextResult(tree: Tree[A, B]) = (tree.key, tree.value)
  }

  private[this] class KeysIterator[A, B](tree: Tree[A, B]) extends TreeIterator[A, B, A](tree) {
    override def nextResult(tree: Tree[A, B]) = tree.key
  }

  private[this] class ValuesIterator[A, B](tree: Tree[A, B]) extends TreeIterator[A, B, B](tree) {
    override def nextResult(tree: Tree[A, B]) = tree.value
  }
}
