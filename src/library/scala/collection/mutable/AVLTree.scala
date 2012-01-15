/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable


/**
 * An immutable AVL Tree implementation used by mutable.TreeSet
 * 
 * @author Lucien Pereira
 * 
 */
private[mutable] sealed trait AVLTree[+A] extends Serializable {
  def balance: Int

  def depth: Int

}

private case class Node[A](val data: A, val left: AVLTree[A], val right: AVLTree[A]) extends AVLTree[A] {
  override val balance: Int = right.depth - left.depth

  override val depth: Int = math.max(left.depth, right.depth) + 1

}

private case object Leaf extends AVLTree[Nothing] {
  override val balance: Int = 0

  override val depth: Int = -1

}

private[mutable] object AVLTree {

  /**
   * Returns a new tree containing the given element.
   * Thows an IllegalArgumentException if element is already present.
   * 
   */
  def insert[A](value: A, tree: AVLTree[A], ordering: Ordering[A]): AVLTree[A] = tree match {
    case Leaf => Node(value, Leaf, Leaf)

    case Node(a, left, right) => if (0 == ordering.compare(value, a)) {
      throw new IllegalArgumentException()
    } else if (-1 == ordering.compare(value, a)) {
      rebalance(Node(a, insert(value, left, ordering), right))
    } else {
      rebalance(Node(a, left, insert(value, right, ordering)))
    }
  }

  def contains[A](value: A, tree: AVLTree[A], ordering: Ordering[A]): Boolean = tree match {
    case Leaf => false

    case Node(a, left, right) => if (0 == ordering.compare(value, a)) {
      true
    } else if (-1 == ordering.compare(value, a)) {
      contains(value, left, ordering)
    } else {
      contains(value, right, ordering)
    }
  }

  /**
   * Return a new tree which not contains given element.
   * 
   */
  def remove[A](value: A, tree: AVLTree[A], ordering: Ordering[A]): AVLTree[A] = tree match {
    case Leaf => throw new NoSuchElementException()

    case Node(a, Leaf, Leaf) => if (0 == ordering.compare(value, a)) {
      Leaf
    } else {
      throw new NoSuchElementException()
    }

    case Node(a, left, right@Node(_, _, _)) => if (0 == ordering.compare(value, a)) {
      val (min, newRight) = removeMin(right)
      rebalance(Node(min, left, newRight))
    } else if (-1 == ordering.compare(value, a)) {
      rebalance(Node(a, remove(value, left, ordering), right))
    } else {
      rebalance(Node(a, left, remove(value, right, ordering)))
    }

    case Node(a, left: Node[A], right) => if (0 == ordering.compare(value, a)) {
      val (max, newLeft) = removeMax(left)
      rebalance(Node(max, newLeft, right))
    } else if (-1 == ordering.compare(value, a)) {
      rebalance(Node(a, remove(value, left, ordering), right))
    } else {
      rebalance(Node(a, left, remove(value, right, ordering)))
    }
  }

  /**
   * Return a tuple containing the biggest element of the provided tree
   * and a new tree from which this element has been extracted.
   * 
   */
  def removeMax[A](tree: AVLTree[A]): (A, AVLTree[A]) = tree match {
    case Node(a, Leaf, Leaf) => (a, Leaf)

    case Node(a, left, Leaf) => (a, left)

    case Node(a, left, right) => {
      val (max, newRight) = removeMax(right)
      (max, rebalance(Node(a, left, newRight)))
    }

    case Leaf => sys.error("Should not happen.")
  }

  /**
   * Return a tuple containing the smallest element of the provided tree
   * and a new tree from which this element has been extracted.
   * 
   */
  def removeMin[A](tree: AVLTree[A]): (A, AVLTree[A]) = tree match {
    case Node(a, Leaf, Leaf) => (a, Leaf)

    case Node(a, Leaf, right) => (a, right)

    case Node(a, left, right) => {
      val (min, newLeft) = removeMin(left)
      (min, rebalance(Node(a, newLeft, right)))
    }

    case Leaf => sys.error("Should not happen.")
  }

  /**
   * Returns a bounded stream of elements in the tree.
   * 
   */
  def toStream[A](tree: AVLTree[A], isLeftAcceptable: A => Boolean, isRightAcceptable: A => Boolean): Stream[A] = tree match {
    case Leaf => Stream.empty

    case Node(a, left, right) => if (isLeftAcceptable(a)) {
      if (isRightAcceptable(a)) {
        toStream(left, isLeftAcceptable, isRightAcceptable) ++ Stream(a) ++ toStream(right, isLeftAcceptable, isRightAcceptable)
      } else {
        toStream(left, isLeftAcceptable, isRightAcceptable)
      }
    } else if (isRightAcceptable(a)) {
      toStream(right, isLeftAcceptable, isRightAcceptable)
    } else {
      Stream.empty
    }
  }

  /**
   * Returns a bounded iterator of elements in the tree.
   * 
   */
  def iterator[A](tree: AVLTree[A], isLeftAcceptable: A => Boolean, isRightAcceptable: A => Boolean): Iterator[A] =
    toStream(tree, isLeftAcceptable, isRightAcceptable).iterator

  def rebalance[A](tree: AVLTree[A]): AVLTree[A] = (tree, tree.balance) match {
    case (node@Node(_, left, _), -2) => left.balance match {
      case 1 => doubleRightRotation(node)
      case _ => rightRotation(node)
    }

    case (node@Node(_, _, right), 2) => right.balance match {
      case -1 => doubleLeftRotation(node)
      case _ => leftRotation(node)
    }

    case _ => tree
  }

  def leftRotation[A](tree: Node[A]): AVLTree[A] = tree.right match {
    case Node(b, left, right) => Node(b, Node(tree.data, tree.left, left), right)
    case _ => sys.error("Should not happen.")
  }

  def rightRotation[A](tree: Node[A]): AVLTree[A] = tree.left match {
    case Node(b, left, right) => Node(b, left, Node(tree.data, right, tree.right))
    case _ => sys.error("Should not happen.")
  }

  def doubleLeftRotation[A](tree: Node[A]): AVLTree[A] = tree.right match {
    case right@Node(b, l, r) => leftRotation(Node(tree.data, tree.left, rightRotation(right)))
    case _ => sys.error("Should not happen.")
  }

  def doubleRightRotation[A](tree: Node[A]): AVLTree[A] = tree.left match {
    case left@Node(b, l, r) => rightRotation(Node(tree.data, leftRotation(left), tree.right))
    case _ => sys.error("Should not happen.")
  }

}
