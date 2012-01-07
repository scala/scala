/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

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
