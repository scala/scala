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

abstract class RedBlackTest extends Properties("RedBlack") {
  def minimumSize = 0
  def maximumSize = 5

  object RedBlackTest extends scala.collection.immutable.RedBlack[String] {
    def isSmaller(x: String, y: String) = x < y
  }
  
  import RedBlackTest._
  
  def nodeAt[A](tree: Tree[A], n: Int): Option[(String, A)] = if (n < tree.iterator.size && n >= 0)
    Some(tree.iterator.drop(n).next)
  else
    None
    
  def treeContains[A](tree: Tree[A], key: String) = tree.iterator.map(_._1) contains key
  
  def mkTree(level: Int, parentIsBlack: Boolean = false, label: String = ""): Gen[Tree[Int]] = 
    if (level == 0) {
      value(Empty)
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
  def genParm(tree: Tree[Int]): Gen[ModifyParm]
  def modify(tree: Tree[Int], parm: ModifyParm): Tree[Int]
  
  def genInput: Gen[(Tree[Int], ModifyParm, Tree[Int])] = for {
    tree <- genTree
    parm <- genParm(tree)
  } yield (tree, parm, modify(tree, parm))
}

trait RedBlackInvariants {
  self: RedBlackTest =>
  
  import RedBlackTest._
    
  def rootIsBlack[A](t: Tree[A]) = t.isBlack
  
  def areAllLeavesBlack[A](t: Tree[A]): Boolean = t match {
    case Empty => t.isBlack
    case ne: NonEmpty[_] => List(ne.left, ne.right) forall areAllLeavesBlack
  }
  
  def areRedNodeChildrenBlack[A](t: Tree[A]): Boolean = t match {
    case RedTree(_, _, left, right) => List(left, right) forall (t => t.isBlack && areRedNodeChildrenBlack(t)) 
    case BlackTree(_, _, left, right) => List(left, right) forall areRedNodeChildrenBlack
    case Empty => true
  }
  
  def blackNodesToLeaves[A](t: Tree[A]): List[Int] = t match {
    case Empty => List(1)
    case BlackTree(_, _, left, right) => List(left, right) flatMap blackNodesToLeaves map (_ + 1)
    case RedTree(_, _, left, right) => List(left, right) flatMap blackNodesToLeaves
  }
  
  def areBlackNodesToLeavesEqual[A](t: Tree[A]): Boolean = t match {
    case Empty => true
    case ne: NonEmpty[_] => 
      (
        blackNodesToLeaves(ne).distinct.size == 1 
        && areBlackNodesToLeavesEqual(ne.left) 
        && areBlackNodesToLeavesEqual(ne.right)
      )
  }
  
  def orderIsPreserved[A](t: Tree[A]): Boolean = 
    t.iterator zip t.iterator.drop(1) forall { case (x, y) => isSmaller(x._1, y._1) }
    
  def setup(invariant: Tree[Int] => Boolean) = forAll(genInput) { case (tree, parm, newTree) =>
    invariant(newTree)
  }

  property("root is black") = setup(rootIsBlack)
  property("all leaves are black") = setup(areAllLeavesBlack)
  property("children of red nodes are black") = setup(areRedNodeChildrenBlack)
  property("black nodes are balanced") = setup(areBlackNodesToLeavesEqual)
  property("ordering of keys is preserved") = setup(orderIsPreserved)
}

object TestInsert extends RedBlackTest with RedBlackInvariants {
  import RedBlackTest._
  
  override type ModifyParm = Int
  override def genParm(tree: Tree[Int]): Gen[ModifyParm] = choose(0, tree.iterator.size + 1)
  override def modify(tree: Tree[Int], parm: ModifyParm): Tree[Int] = tree update (generateKey(tree, parm), 0)

  def generateKey(tree: Tree[Int], parm: ModifyParm): String = nodeAt(tree, parm) match {
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

object TestModify extends RedBlackTest {
  import RedBlackTest._
  
  def newValue = 1
  override def minimumSize = 1
  override type ModifyParm = Int
  override def genParm(tree: Tree[Int]): Gen[ModifyParm] = choose(0, tree.iterator.size)
  override def modify(tree: Tree[Int], parm: ModifyParm): Tree[Int] = nodeAt(tree, parm) map { 
    case (key, _) => tree update (key, newValue)
  } getOrElse tree

  property("update modifies values") = forAll(genInput) { case (tree, parm, newTree) =>
    nodeAt(tree,parm) forall { case (key, _) =>
      newTree.iterator contains (key, newValue)
    }
  }
}

object TestDelete extends RedBlackTest with RedBlackInvariants  {
  import RedBlackTest._

  override def minimumSize = 1
  override type ModifyParm = Int
  override def genParm(tree: Tree[Int]): Gen[ModifyParm] = choose(0, tree.iterator.size)
  override def modify(tree: Tree[Int], parm: ModifyParm): Tree[Int] = nodeAt(tree, parm) map { 
    case (key, _) => tree delete key
  } getOrElse tree
  
  property("delete removes elements") = forAll(genInput) { case (tree, parm, newTree) =>
    nodeAt(tree, parm) forall { case (key, _) =>
      !treeContains(newTree, key)
    }
  }
}

object TestRange extends RedBlackTest with RedBlackInvariants  {
  import RedBlackTest._
  
  override type ModifyParm = (Option[Int], Option[Int])
  override def genParm(tree: Tree[Int]): Gen[ModifyParm] = for {
    from <- choose(0, tree.iterator.size)
    to <- choose(0, tree.iterator.size) suchThat (from <=)
    optionalFrom <- oneOf(Some(from), None, Some(from)) // Double Some(n) to get around a bug
    optionalTo <- oneOf(Some(to), None, Some(to)) // Double Some(n) to get around a bug
  } yield (optionalFrom, optionalTo)
  
  override def modify(tree: Tree[Int], parm: ModifyParm): Tree[Int] = {
    val from = parm._1 flatMap (nodeAt(tree, _) map (_._1))
    val to = parm._2 flatMap (nodeAt(tree, _) map (_._1))
    tree range (from, to)
  }
  
  property("range boundaries respected") = forAll(genInput) { case (tree, parm, newTree) =>
    val from = parm._1 flatMap (nodeAt(tree, _) map (_._1))
    val to = parm._2 flatMap (nodeAt(tree, _) map (_._1))
    ("lower boundary" |: (from forall ( key => newTree.iterator.map(_._1) forall (key <=)))) &&
    ("upper boundary" |: (to forall ( key => newTree.iterator.map(_._1) forall (key >))))
  }
  
  property("range returns all elements") = forAll(genInput) { case (tree, parm, newTree) =>
    val from = parm._1 flatMap (nodeAt(tree, _) map (_._1))
    val to = parm._2 flatMap (nodeAt(tree, _) map (_._1))
    val filteredTree = (tree.iterator
      .map(_._1) 
      .filter(key => from forall (key >=))
      .filter(key => to forall (key <))
      .toList)
    filteredTree == newTree.iterator.map(_._1).toList
  }
}

object Test extends Properties("RedBlack") {
  include(TestInsert)
  include(TestModify)
  include(TestDelete)
  include(TestRange)
}

