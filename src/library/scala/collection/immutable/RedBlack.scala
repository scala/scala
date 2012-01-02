/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

/** An object containing the RedBlack tree implementation used by for `TreeMaps` and `TreeSets`.
 *
 *  @since 2.3
 */
private[immutable]
object RedBlack {

  private def blacken[A, B](t: Tree[A, B]): Tree[A, B] = t.black

  private def mkTree[A, B](isBlack: Boolean, k: A, v: B, l: Tree[A, B], r: Tree[A, B]) =
    if (isBlack) BlackTree(k, v, l, r) else RedTree(k, v, l, r)
  def isRedTree[A, B](tree: Tree[A, B]) = tree.isInstanceOf[RedTree[_, _]]
  def isBlackTree(tree: Tree[_, _]) = tree.isInstanceOf[BlackTree[_, _]]

  @annotation.tailrec
  def lookup[A, B](tree: Tree[A, B], x: A)(implicit ordering: Ordering[A]): Tree[A, B] = if (tree eq Empty.Instance) tree else {
      val cmp = ordering.compare(x, tree.key)
      if (cmp < 0) lookup(tree.left, x)
      else if (cmp > 0) lookup(tree.right, x)
      else tree
  }
  sealed abstract class Tree[A, +B] extends Serializable {
    def key: A
    def value: B
    def left: Tree[A, B]
    def right: Tree[A, B]
    def isEmpty: Boolean
    def isBlack: Boolean
    def lookup(x: A)(implicit ordering: Ordering[A]): Tree[A, B]
    def update[B1 >: B](k: A, v: B1)(implicit ordering: Ordering[A]): Tree[A, B1] = blacken(upd(k, v))
    def delete(k: A)(implicit ordering: Ordering[A]): Tree[A, B] = blacken(del(k))
    def range(from: Option[A], until: Option[A])(implicit ordering: Ordering[A]): Tree[A, B] = blacken(rng(from, until))
    def foreach[U](f: ((A, B)) =>  U)
    def foreachKey[U](f: A =>  U)
    def iterator: Iterator[(A, B)]
    def keyIterator: Iterator[A]
    def upd[B1 >: B](k: A, v: B1)(implicit ordering: Ordering[A]): Tree[A, B1]
    def del(k: A)(implicit ordering: Ordering[A]): Tree[A, B]
    def smallest: NonEmpty[A, B]
    def greatest: NonEmpty[A, B]
    def rng(from: Option[A], until: Option[A])(implicit ordering: Ordering[A]): Tree[A, B]
    def first : A
    def last : A
    def count : Int
    protected[immutable] def nth(n: Int): NonEmpty[A, B]
    def black: Tree[A, B] = this
    def red: Tree[A, B]
  }
  sealed abstract class NonEmpty[A, +B](final val key: A, final val value: B, final val left: Tree[A, B], final val right: Tree[A, B]) extends Tree[A, B] with Serializable {
    def isEmpty = false
    def lookup(k: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
      val cmp = ordering.compare(k, key)
      if (cmp < 0) left.lookup(k)
      else if (cmp > 0) right.lookup(k)
      else this
    }
    private[this] def balanceLeft[B1 >: B](isBlack: Boolean, z: A, zv: B, l: Tree[A, B1], d: Tree[A, B1])/*: NonEmpty[A, B1]*/ = {
      if (isRedTree(l) && isRedTree(l.left))
        RedTree(l.key, l.value, BlackTree(l.left.key, l.left.value, l.left.left, l.left.right), BlackTree(z, zv, l.right, d))
      else if (isRedTree(l) && isRedTree(l.right))
        RedTree(l.right.key, l.right.value, BlackTree(l.key, l.value, l.left, l.right.left), BlackTree(z, zv, l.right.right, d))
      else
        mkTree(isBlack, z, zv, l, d)
    }
    private[this] def balanceRight[B1 >: B](isBlack: Boolean, x: A, xv: B, a: Tree[A, B1], r: Tree[A, B1])/*: NonEmpty[A, B1]*/ = {
      if (isRedTree(r) && isRedTree(r.left))
        RedTree(r.left.key, r.left.value, BlackTree(x, xv, a, r.left.left), BlackTree(r.key, r.value, r.left.right, r.right))
      else if (isRedTree(r) && isRedTree(r.right))
        RedTree(r.key, r.value, BlackTree(x, xv, a, r.left), BlackTree(r.right.key, r.right.value, r.right.left, r.right.right))
      else
        mkTree(isBlack, x, xv, a, r)
    }
    def upd[B1 >: B](k: A, v: B1)(implicit ordering: Ordering[A]): Tree[A, B1] = {
      val cmp = ordering.compare(k, key)
      if (cmp < 0) balanceLeft(isBlack, key, value, left.upd(k, v), right)
      else if (cmp > 0) balanceRight(isBlack, key, value, left, right.upd(k, v))
      else mkTree(isBlack, k, v, left, right)
    }
    // Based on Stefan Kahrs' Haskell version of Okasaki's Red&Black Trees
    // http://www.cse.unsw.edu.au/~dons/data/RedBlackTree.html
    def del(k: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
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
        sys.error("Defect: invariance violation at "+right)
      }
      def balRight(x: A, xv: B, tl: Tree[A, B], tr: Tree[A, B]) = if (isRedTree(tr)) {
        RedTree(x, xv, tl, tr.black)
      } else if (isBlackTree(tl)) {
        balance(x, xv, tl.red, tr)
      } else if (isRedTree(tl) && isBlackTree(tl.right)) {
        RedTree(tl.right.key, tl.right.value, balance(tl.key, tl.value, subl(tl.left), tl.right.left), BlackTree(x, xv, tl.right.right, tr))
      } else {
        sys.error("Defect: invariance violation at "+left)
      }
      def delLeft = if (isBlackTree(left)) balLeft(key, value, left.del(k), right) else RedTree(key, value, left.del(k), right)
      def delRight = if (isBlackTree(right)) balRight(key, value, left, right.del(k)) else RedTree(key, value, left, right.del(k))
      def append(tl: Tree[A, B], tr: Tree[A, B]): Tree[A, B] = if (tl eq Empty.Instance) {
        tr
      } else if (tr eq Empty.Instance) {
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

      val cmp = ordering.compare(k, key)
      if (cmp < 0) delLeft
      else if (cmp > 0) delRight
      else append(left, right)
    }

    def smallest: NonEmpty[A, B] = if (left  eq Empty.Instance) this else left.smallest
    def greatest: NonEmpty[A, B] = if (right eq Empty.Instance) this else right.greatest

    def iterator: Iterator[(A, B)] = new TreeIterator(this)
    def keyIterator: Iterator[A] = new TreeKeyIterator(this)

    override def foreach[U](f: ((A, B)) => U) {
      if (left ne Empty.Instance) left foreach f
      f((key, value))
      if (right ne Empty.Instance) right foreach f
    }

    override def foreachKey[U](f: A => U) {
      if (left ne Empty.Instance) left foreachKey f
      f(key)
      if (right ne Empty.Instance) right foreachKey f
    }

    override def rng(from: Option[A], until: Option[A])(implicit ordering: Ordering[A]): Tree[A, B] = {
      if (from == None && until == None) return this
      if (from != None && ordering.lt(key, from.get)) return right.rng(from, until);
      if (until != None && ordering.lteq(until.get, key)) return left.rng(from, until);
      val newLeft = left.rng(from, None)
      val newRight = right.rng(None, until)
      if ((newLeft eq left) && (newRight eq right)) this
      else if (newLeft eq Empty.Instance) newRight.upd(key, value);
      else if (newRight eq Empty.Instance) newLeft.upd(key, value);
      else rebalance(newLeft, newRight)
    }

    // The zipper returned might have been traversed left-most (always the left child)
    // or right-most (always the right child). Left trees are traversed right-most,
    // and right trees are traversed leftmost.

    // Returns the zipper for the side with deepest black nodes depth, a flag
    // indicating whether the trees were unbalanced at all, and a flag indicating
    // whether the zipper was traversed left-most or right-most.

    // If the trees were balanced, returns an empty zipper
    private[this] def compareDepth(left: Tree[A, B], right: Tree[A, B]): (List[NonEmpty[A, B]], Boolean, Boolean, Int) = {
      // Once a side is found to be deeper, unzip it to the bottom
      def unzip(zipper: List[NonEmpty[A, B]], leftMost: Boolean): List[NonEmpty[A, B]] = {
        val next = if (leftMost) zipper.head.left else zipper.head.right
        next match {
          case node: NonEmpty[_, _] => unzip(node :: zipper, leftMost)
          case _                    => zipper
        }
      }

      // Unzip left tree on the rightmost side and right tree on the leftmost side until one is
      // found to be deeper, or the bottom is reached
      def unzipBoth(left: Tree[A, B],
                    right: Tree[A, B],
                    leftZipper: List[NonEmpty[A, B]],
                    rightZipper: List[NonEmpty[A, B]],
                    smallerDepth: Int): (List[NonEmpty[A, B]], Boolean, Boolean, Int) = (left, right) match {
        case (l @ BlackTree(_, _, _, _), r @ BlackTree(_, _, _, _)) =>
          unzipBoth(l.right, r.left, l :: leftZipper, r :: rightZipper, smallerDepth + 1)
        case (l @ RedTree(_, _, _, _), r @ RedTree(_, _, _, _)) =>
          unzipBoth(l.right, r.left, l :: leftZipper, r :: rightZipper, smallerDepth)
        case (_, r @ RedTree(_, _, _, _)) =>
          unzipBoth(left, r.left, leftZipper, r :: rightZipper, smallerDepth)
        case (l @ RedTree(_, _, _, _), _) =>
          unzipBoth(l.right, right, l :: leftZipper, rightZipper, smallerDepth)
        case (Empty.Instance, Empty.Instance) =>
          (Nil, true, false, smallerDepth)
        case (Empty.Instance, r @ BlackTree(_, _, _, _)) =>
          val leftMost = true
          (unzip(r :: rightZipper, leftMost), false, leftMost, smallerDepth)
        case (l @ BlackTree(_, _, _, _), Empty.Instance) =>
          val leftMost = false
          (unzip(l :: leftZipper, leftMost), false, leftMost, smallerDepth)
      }
      unzipBoth(left, right, Nil, Nil, 0)
    }

    private[this] def rebalance(newLeft: Tree[A, B], newRight: Tree[A, B]) = {
      // This is like drop(n-1), but only counting black nodes
      def  findDepth(zipper: List[NonEmpty[A, B]], depth: Int): List[NonEmpty[A, B]] = zipper match {
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
        val zippedTree = zipFrom.tail.foldLeft(union: Tree[A, B]) { (tree, node) =>
            if (leftMost)
              balanceLeft(node.isBlack, node.key, node.value, tree, node.right)
            else
              balanceRight(node.isBlack, node.key, node.value, node.left, tree)
        }
        zippedTree
      }
    }
    def first = if (left  eq Empty.Instance) key else left.first
    def last  = if (right eq Empty.Instance) key else right.last
    val count = 1 + left.count + right.count
    protected[immutable] def nth(n: Int) = {
      val count = left.count
      if (n < count) left.nth(n)
      else if (n > count) right.nth(n - count - 1)
      else this
    }
  }
  object Empty {
    def empty[A]: Tree[A, Nothing] = Instance.asInstanceOf[Tree[A, Nothing]]

    final val Instance: Tree[_ >: Nothing, Nothing] = Empty[Nothing]()
  }
  final case class Empty[A] private () extends Tree[A, Nothing] {
    def key = throw new NoSuchElementException("empty map")
    def value = throw new NoSuchElementException("empty map")
    def left = this
    def right = this
    def isEmpty = true
    def isBlack = true
    def lookup(k: A)(implicit ordering: Ordering[A]): Tree[A, Nothing] = this
    def upd[B](k: A, v: B)(implicit ordering: Ordering[A]): Tree[A, B] = RedTree(k, v, this, this)
    def del(k: A)(implicit ordering: Ordering[A]): Tree[A, Nothing] = this
    def smallest: NonEmpty[A, Nothing] = throw new NoSuchElementException("empty map")
    def greatest: NonEmpty[A, Nothing] = throw new NoSuchElementException("empty map")
    def iterator: Iterator[(A, Nothing)] = Iterator.empty
    def keyIterator: Iterator[A] = Iterator.empty

    override def foreach[U](f: ((A, Nothing)) => U) {}
    override def foreachKey[U](f: A => U) {}

    def rng(from: Option[A], until: Option[A])(implicit ordering: Ordering[A]) = this
    def first = throw new NoSuchElementException("empty map")
    def last = throw new NoSuchElementException("empty map")
    def count = 0
    protected[immutable] def nth(n: Int) = throw new NoSuchElementException("empty map")
    override def red = sys.error("cannot make leaf red")

    override def toString() = "Empty"

    private def readResolve() = Empty.empty
  }
  final class RedTree[A, +B](key: A,
                         value: B,
                         left: Tree[A, B],
                         right: Tree[A, B]) extends NonEmpty[A, B](key, value, left, right) {
    def isBlack = false
    override def black = BlackTree(key, value, left, right)
    override def red = this
  }
  object RedTree {
    def apply[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = new RedTree(key, value, left, right)
    def unapply[A, B](t: RedTree[A, B]) = Some((t.key, t.value, t.left, t.right))
  }
  final class BlackTree[A, +B](key: A,
                           value: B,
                           left: Tree[A, B],
                           right: Tree[A, B]) extends NonEmpty[A, B](key, value, left, right) {
    def isBlack = true
    override def red = RedTree(key, value, left, right)
  }
  object BlackTree {
    def apply[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = new BlackTree(key, value, left, right)
    def unapply[A, B](t: BlackTree[A, B]) = Some((t.key, t.value, t.left, t.right))
  }

  private[this] class TreeIterator[A, B](tree: NonEmpty[A, B]) extends Iterator[(A, B)] {
    override def hasNext: Boolean = next ne Empty.Instance

    override def next: (A, B) = next match {
      case Empty.Instance =>
        throw new NoSuchElementException("next on empty iterator")
      case tree: NonEmpty[A, B] =>
        addLeftMostBranchToPath(tree.right)
        next = if (path.isEmpty) Empty.empty else path.pop()
        (tree.key, tree.value)
    }

    @annotation.tailrec
    private[this] def addLeftMostBranchToPath(tree: Tree[A, B]) {
      tree match {
        case Empty.Instance =>
        case tree: NonEmpty[A, B] =>
          path.push(tree)
          addLeftMostBranchToPath(tree.left)
      }
    }

    private[this] val path = mutable.ArrayStack.empty[NonEmpty[A, B]]
    addLeftMostBranchToPath(tree)
    private[this] var next: Tree[A, B] = path.pop()
  }

  private[this] class TreeKeyIterator[A](tree: NonEmpty[A, _]) extends Iterator[A] {
    override def hasNext: Boolean = next ne Empty.Instance

    override def next: A = next match {
      case Empty.Instance =>
        throw new NoSuchElementException("next on empty iterator")
      case tree: NonEmpty[A, _] =>
        addLeftMostBranchToPath(tree.right)
        next = if (path.isEmpty) Empty.empty else path.pop()
        tree.key
    }

    @annotation.tailrec
    private[this] def addLeftMostBranchToPath(tree: Tree[A, _]) {
      tree match {
        case Empty.Instance =>
        case tree: NonEmpty[A, _] =>
          path.push(tree)
          addLeftMostBranchToPath(tree.left)
      }
    }

    private[this] val path = mutable.ArrayStack.empty[NonEmpty[A, _]]
    addLeftMostBranchToPath(tree)
    private[this] var next: Tree[A, _] = path.pop()
  }
}
