package scala.collection.mutable.redblacktree

import collection.mutable.{RedBlackTree => RB}
import org.scalacheck._
import Prop._
import Gen._

/*
Properties of a Red & Black Tree:

A node is either red or black.
The root is black. (This rule is used in some definitions and not others. Since the
root can always be changed from red to black but not necessarily vice-versa this
rule has little effect on analysis.)
All leaves are black.
Both children of every red node are black.
Every simple path from a given node to any of its descendant leaves contains the same number of black nodes.
*/

abstract class RedBlackTreeTest(tname: String) extends Properties(tname) with RedBlackTreeInvariants[String, Int] {
  def minimumSize = 0
  def maximumSize = 5

  import RB._

  def nodeAt[A](tree: Tree[String, A], n: Int): Option[(String, A)] = if (n < iterator(tree).size && n >= 0)
    Some(iterator(tree).drop(n).next())
  else
    None

  def treeContains[A](tree: Tree[String, A], key: String) = iterator(tree).map(_._1) contains key

  def height(node: Node[_, _]): Int = if (node eq null) 0 else (1 + math.max(height(node.left), height(node.right)))

  def RedTree[A, B](k: A, v: B, l: Tree[A, B], r: Tree[A, B]): Tree[A, B] = {
    val n = new Node(k, v, true, l.root, r.root, null)
    if(l.root ne null) l.root.parent = n
    if(r.root ne null) r.root.parent = n
    new Tree(n, l.size + r.size + 1)
  }

  def BlackTree[A, B](k: A, v: B, l: Tree[A, B], r: Tree[A, B]): Tree[A, B] = {
    val n = new Node(k, v, false, l.root, r.root, null)
    if(l.root ne null) l.root.parent = n
    if(r.root ne null) r.root.parent = n
    new Tree(n, l.size + r.size + 1)
  }

  def mkTree(level: Int, parentIsBlack: Boolean = false, label: String = ""): Gen[Tree[String, Int]] =
    if (level == 0) {
      const(new Tree(null, 0))
    } else {
      for {
        oddOrEven <- choose(0, 2)
        tryRed = oddOrEven.sample.get % 2 == 0 // work around arbitrary[Boolean] bug
        isRed = parentIsBlack && tryRed
        nextLevel = if (isRed) level else level - 1
        left <- mkTree(nextLevel, !isRed, label + "L")
        right <- mkTree(nextLevel, !isRed, label + "R")
      } yield {
        if (isRed)
          RedTree(label + "N", 0, left, right)
        else
          BlackTree(label + "N", 0, left, right)
      }
    }

  def genTree = for {
    depth <- choose(minimumSize, maximumSize + 1)
    tree <- mkTree(depth)
  } yield tree

  type ModifyParm
  def genParm(tree: Tree[String, Int]): Gen[ModifyParm]
  def modify(tree: Tree[String, Int], parm: ModifyParm): Unit

  def genInput: Gen[(Tree[String, Int], ModifyParm, Tree[String, Int])] = for {
    tree <- genTree
    parm <- genParm(tree)
  } yield (tree, parm, { val c = tree.treeCopy(); modify(c, parm); c })

  def setup(invariant: Tree[String, Int] => Boolean): Prop = forAll(genInput) { case (tree, parm, newTree) =>
    invariant(newTree)
  }

  implicit def ordering: Ordering[String] = Ordering.String
}

trait RedBlackTreeInvariants[K, V] {
  self: Properties =>

  import RB._

  implicit def ordering: Ordering[K]

  def rootIsBlack(t: Tree[K, V]) = isBlack(t.root)

  def areAllLeavesBlack(t: Tree[K, V]): Boolean = areAllLeavesBlack(t.root)

  def areAllLeavesBlack(n: Node[K, V]): Boolean = n match {
    case null => isBlack(n)
    case ne => List(ne.left, ne.right) forall areAllLeavesBlack
  }

  def areRedNodeChildrenBlack(t: Tree[K, V]): Boolean = areRedNodeChildrenBlack(t.root)

  def areRedNodeChildrenBlack(n: Node[K, V]): Boolean = n match {
    case null => true
    case n if n.red => List(n.left, n.right) forall (t => isBlack(t) && areRedNodeChildrenBlack(t))
    case n => List(n.left, n.right) forall areRedNodeChildrenBlack
  }

  def blackNodesToLeaves(n: Node[K, V]): List[Int] = n match {
    case null => List(1)
    case n if n.red => List(n.left, n.right) flatMap blackNodesToLeaves
    case n => List(n.left, n.right) flatMap blackNodesToLeaves map (_ + 1)
  }

  def areBlackNodesToLeavesEqual(t: Tree[K, V]): Boolean = areBlackNodesToLeavesEqual(t.root)

  def areBlackNodesToLeavesEqual(n: Node[K, V]): Boolean = n match {
    case null => true
    case ne =>
      blackNodesToLeaves(ne).distinct.size == 1 &&
        areBlackNodesToLeavesEqual(ne.left) &&
        areBlackNodesToLeavesEqual(ne.right)
  }

  def orderIsPreserved(t: Tree[K, V]): Boolean =
    iterator(t) zip iterator(t).drop(1) forall { case (x, y) => ordering.compare(x._1, y._1) < 0 }

  def sizeIsConsistent(t: Tree[K, V]): Boolean =
    size(t) == size(t.root)

  def setup(invariant: Tree[K, V] => Boolean): Prop

  property("root is black") = setup(rootIsBlack)
  property("all leaves are black") = setup(areAllLeavesBlack)
  property("children of red nodes are black") = setup(areRedNodeChildrenBlack)
  property("black nodes are balanced") = setup(areBlackNodesToLeavesEqual)
  property("ordering of keys is preserved") = setup(orderIsPreserved)
  property("size is consistent") = setup(sizeIsConsistent)
}

object TestInsert extends RedBlackTreeTest("RedBlackTree.insert") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, iterator(tree).size + 1)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Unit = insert(tree, generateKey(tree, parm), 0)

  def generateKey(tree: Tree[String, Int], parm: ModifyParm): String = nodeAt(tree, parm) match {
    case Some((key, _)) => key.init.mkString + "MN"
    case None => nodeAt(tree, parm - 1) match {
      case Some((key, _)) => key.init.mkString + "RN"
      case None  => "N"
    }
  }

  property("update adds elements") = forAll(genInput) { case (tree, parm, newTree) =>
    treeContains(newTree, generateKey(tree, parm))
  }
}

object TestModify extends RedBlackTreeTest("RedBlackTree.modify") {
  import RB._

  def newValue = 1
  override def minimumSize = 1
  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, iterator(tree).size)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Unit = nodeAt(tree, parm) foreach {
    case (key, _) => insert(tree, key, newValue)
  }

  property("update modifies values") = forAll(genInput) { case (tree, parm, newTree) =>
    nodeAt(tree,parm) forall { case (key, _) =>
      iterator(newTree) contains (key, newValue)
    }
  }
}

object TestDelete extends RedBlackTreeTest("RedBlackTree.delete") {
  import RB._

  override def minimumSize = 1
  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, iterator(tree).size)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Unit = nodeAt(tree, parm) foreach {
    case (key, _) => delete(tree, key)
  }

  property("delete removes elements") = forAll(genInput) { case (tree, parm, newTree) =>
    nodeAt(tree, parm) forall { case (key, _) =>
      !treeContains(newTree, key)
    }
  }
}

object TestTransform extends RedBlackTreeTest("RedBlackTree.transform") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, size(tree))
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Unit =
    transform[String, Int](tree, (k, v) => if(v < parm) v else v+1)

  property("transform") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).toList.map { case (k, v) => if(v < parm) (k, v) else (k, v+1) } == iterator(newTree).toList
  }
}
