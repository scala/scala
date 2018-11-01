package scala.collection.immutable.redblacktree

import collection.immutable.{RedBlackTree => RB}
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
    Some(iterator(tree).drop(n).next)
  else
    None

  def treeContains[A](tree: Tree[String, A], key: String) = iterator(tree).map(_._1) contains key

  def height(tree: Tree[_, _]): Int = if (tree eq null) 0 else (1 + math.max(height(tree.left), height(tree.right)))

  def mkTree(level: Int, parentIsBlack: Boolean = false, label: String = ""): Gen[Tree[String, Int]] =
    if (level == 0) {
      const(null)
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
  def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int]

  def genInput: Gen[(Tree[String, Int], ModifyParm, Tree[String, Int])] = for {
    tree <- genTree
    parm <- genParm(tree)
  } yield (tree, parm, modify(tree, parm))

  def setup(invariant: Tree[String, Int] => Boolean): Prop = forAll(genInput) { case (tree, parm, newTree) =>
    invariant(newTree)
  }

  implicit def ordering: Ordering[String] = Ordering.String
}

trait RedBlackTreeInvariants[K, V] {
  self: Properties =>

  import RB._

  implicit def ordering: Ordering[K]

  def rootIsBlack[A](t: Tree[K, V]) = isBlack(t)

  def areAllLeavesBlack[A](t: Tree[K, V]): Boolean = t match {
    case null => isBlack(t)
    case ne => List(ne.left, ne.right) forall areAllLeavesBlack
  }

  def areRedNodeChildrenBlack[A](t: Tree[K, V]): Boolean = t match {
    case RedTree(_, _, left, right) => List(left, right) forall (t => isBlack(t) && areRedNodeChildrenBlack(t))
    case BlackTree(_, _, left, right) => List(left, right) forall areRedNodeChildrenBlack
    case null => true
  }

  def blackNodesToLeaves[A](t: Tree[K, V]): List[Int] = t match {
    case null => List(1)
    case BlackTree(_, _, left, right) => List(left, right) flatMap blackNodesToLeaves map (_ + 1)
    case RedTree(_, _, left, right) => List(left, right) flatMap blackNodesToLeaves
  }

  def areBlackNodesToLeavesEqual[A](t: Tree[K, V]): Boolean = t match {
    case null => true
    case ne =>
      (
        blackNodesToLeaves(ne).distinct.size == 1
          && areBlackNodesToLeavesEqual(ne.left)
          && areBlackNodesToLeavesEqual(ne.right)
        )
  }

  def orderIsPreserved[A](t: Tree[K, V]): Boolean =
    iterator(t) zip iterator(t).drop(1) forall { case (x, y) => ordering.compare(x._1, y._1) < 0 }

  def setup(invariant: Tree[K, V] => Boolean): Prop

  property("root is black") = setup(rootIsBlack)
  property("all leaves are black") = setup(areAllLeavesBlack)
  property("children of red nodes are black") = setup(areRedNodeChildrenBlack)
  property("black nodes are balanced") = setup(areBlackNodesToLeavesEqual)
  property("ordering of keys is preserved") = setup(orderIsPreserved)
}

object TestInsert extends RedBlackTreeTest("RedBlackTree.insert") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, iterator(tree).size + 1)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = update(tree, generateKey(tree, parm), 0, true)

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
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = nodeAt(tree, parm) map {
    case (key, _) => update(tree, key, newValue, true)
  } getOrElse tree

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
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = nodeAt(tree, parm) map {
    case (key, _) => delete(tree, key)
  } getOrElse tree

  property("delete removes elements") = forAll(genInput) { case (tree, parm, newTree) =>
    nodeAt(tree, parm) forall { case (key, _) =>
      !treeContains(newTree, key)
    }
  }
}

object TestRange extends RedBlackTreeTest("RedBlackTree.range") {
  import RB._

  override type ModifyParm = (Option[Int], Option[Int])
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = for {
    from <- choose(0, iterator(tree).size)
    to <- choose(0, iterator(tree).size) suchThat (from <= _)
    optionalFrom <- oneOf(Some(from), None, Some(from)) // Double Some(n) to get around a bug
    optionalTo <- oneOf(Some(to), None, Some(to)) // Double Some(n) to get around a bug
  } yield (optionalFrom, optionalTo)

  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = {
    val from = parm._1 flatMap (nodeAt(tree, _) map (_._1))
    val to = parm._2 flatMap (nodeAt(tree, _) map (_._1))
    rangeImpl(tree, from, to)
  }

  property("range boundaries respected") = forAll(genInput) { case (tree, parm, newTree) =>
    val from = parm._1 flatMap (nodeAt(tree, _) map (_._1))
    val to = parm._2 flatMap (nodeAt(tree, _) map (_._1))
    ("lower boundary" |: (from forall ( key => keysIterator(newTree) forall (key <=)))) &&
    ("upper boundary" |: (to forall ( key => keysIterator(newTree) forall (key >))))
  }

  property("range returns all elements") = forAll(genInput) { case (tree, parm, newTree) =>
    val from = parm._1 flatMap (nodeAt(tree, _) map (_._1))
    val to = parm._2 flatMap (nodeAt(tree, _) map (_._1))
    val filteredTree = (keysIterator(tree)
      .filter(key => from forall (key >= _))
      .filter(key => to forall (key < _))
      .toList)
    filteredTree == keysIterator(newTree).toList
  }
}

object TestDrop extends RedBlackTreeTest("RedBlackTree.drop") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, iterator(tree).size)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = drop(tree, parm)

  property("drop") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).drop(parm).toList == iterator(newTree).toList
  }
}

object TestTail extends RedBlackTreeTest("RedBlackTree.tail") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, 0)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] =
    if(tree eq null) null else tail(tree)

  property("tail") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).drop(1).toList == iterator(newTree).toList
  }
}

object TestInit extends RedBlackTreeTest("RedBlackTree.init") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, 0)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] =
    if(tree eq null) null else init(tree)

  property("tail") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).toList.dropRight(1) == iterator(newTree).toList
  }
}

object TestTake extends RedBlackTreeTest("RedBlackTree.take") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, iterator(tree).size)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = take(tree, parm)

  property("take") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).take(parm).toList == iterator(newTree).toList
  }
}

object TestSlice extends RedBlackTreeTest("RedBlackTree.slice") {
  import RB._

  override type ModifyParm = (Int, Int)
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = for {
    from <- choose(0, iterator(tree).size)
    to <- choose(from, iterator(tree).size)
  } yield (from, to)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = slice(tree, parm._1, parm._2)

  property("slice") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).slice(parm._1, parm._2).toList == iterator(newTree).toList
  }
}

object TestFromOrderedKeys extends Properties("RedBlackTree.fromOrderedKeys") with RedBlackTreeInvariants[Int, Null] {
  import RB._

  def modify(s: Set[Int]): Tree[Int, Null] = {
    val xs = s.toIndexedSeq.sorted
    fromOrderedKeys(xs.iterator, xs.size)
  }

  property("fromOrderedKeys") = forAll { (s: Set[Int]) =>
    s.toIndexedSeq.sorted == keysIterator(modify(s)).toIndexedSeq
  }

  def setup(invariant: Tree[Int, Null] => Boolean) = forAll { (s: Set[Int]) =>
    invariant(modify(s))
  }

  implicit def ordering: Ordering[Int] = Ordering.Int
}

object TestFromOrderedEntries extends Properties("RedBlackTree.fromOrderedEntries") with RedBlackTreeInvariants[Int, String] {
  import RB._

  def modify(s: Set[Int]): Tree[Int, String] = {
    val xs = s.toIndexedSeq.sorted.map(i => (i, i.toString))
    fromOrderedEntries(xs.iterator, xs.size)
  }

  property("fromOrderedKeys") = forAll { (s: Set[Int]) =>
    s.toIndexedSeq.sorted.map(i => (i, i.toString)) == iterator(modify(s)).toIndexedSeq
  }

  def setup(invariant: Tree[Int, String] => Boolean) = forAll { (s: Set[Int]) =>
    invariant(modify(s))
  }

  implicit def ordering: Ordering[Int] = Ordering.Int
}

abstract class BulkTest(pName: String) extends RedBlackTreeTest(pName) {
  import RB._

  override type ModifyParm = Tree[String, Int]
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = genTree
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = treeOp(tree, parm)

  def treeOp(t1: Tree[String, Int], t2: Tree[String, Int]): Tree[String, Int]
  def setOp(s1: Set[(String, Int)], s2: Set[(String, Int)]): Set[(String, Int)]

  // Using our own setup here because genTree can't minimize - great when it works but useless when it fails
  // and you have to debug it

  def gen(l1: List[Int], l2: List[Int]) = {
    var t1: Tree[String, Int] = null
    l1.foreach { case i => t1 = update(t1, ""+i, i, false) }
    var t2: Tree[String, Int] = null
    l2.foreach { case i => t2 = update(t2, ""+i, i, false) }
    val t3 = modify(t1, t2)
    (t1, t2, t3)
  }

  override def setup(invariant: Tree[String, Int] => Boolean) = forAll { (l1: List[Int], l2: List[Int]) =>
    val (t1, t2, t3) = gen(l1, l2)
    invariant(t3)
  }

  property(pName) = forAll { (l1: List[Int], l2: List[Int]) =>
    val (t1, t2, t3) = gen(l1, l2)
    setOp(iterator(t1).toSet, iterator(t2).toSet).toList.sorted == iterator(t3).toList
  }
}

object TestUnion extends BulkTest("RedBlackTree.union") {
  import RB._
  def treeOp(t1: Tree[String, Int], t2: Tree[String, Int]): Tree[String, Int] = union(t1, t2)
  def setOp(s1: Set[(String, Int)], s2: Set[(String, Int)]): Set[(String, Int)] = s1.union(s2)
}

object TestIntersect extends BulkTest("RedBlackTree.intersect") {
  import RB._
  def treeOp(t1: Tree[String, Int], t2: Tree[String, Int]): Tree[String, Int] = intersect(t1, t2)
  def setOp(s1: Set[(String, Int)], s2: Set[(String, Int)]): Set[(String, Int)] = s1.intersect(s2)
}

object TestDifference extends BulkTest("RedBlackTree.difference") {
  import RB._
  def treeOp(t1: Tree[String, Int], t2: Tree[String, Int]): Tree[String, Int] = difference(t1, t2)
  def setOp(s1: Set[(String, Int)], s2: Set[(String, Int)]): Set[(String, Int)] = s1.diff(s2)
}

object TestFilter extends RedBlackTreeTest("RedBlackTree.filterEntries") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, 0)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] =
    filterEntries[String, Int](tree, (k, _) => k.hashCode % 2 == 0)

  property("filter") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).filter(t => t._1.hashCode % 2 == 0).toList == iterator(newTree).toList
  }
}

object TestPartitionLeft extends RedBlackTreeTest("RedBlackTree.partitionKeysLeft") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, 0)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] =
    partitionKeys[String, Int](tree, k => k.hashCode % 2 == 0)._1

  property("partition") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).filter(t => t._1.hashCode % 2 == 0).toList == iterator(newTree).toList
  }
}

object TestPartitionRight extends RedBlackTreeTest("RedBlackTree.partitionKeysRight") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, 0)
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] =
    partitionKeys[String, Int](tree, k => k.hashCode % 2 == 0)._2

  property("partition") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).filter(t => t._1.hashCode % 2 != 0).toList == iterator(newTree).toList
  }
}

object TestTransform extends RedBlackTreeTest("RedBlackTree.transform") {
  import RB._

  override type ModifyParm = Int
  override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, count(tree))
  override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] =
    transform[String, Int, Int](tree, (k, v) => if(v < parm) v else v+1)

  property("transform") = forAll(genInput) { case (tree, parm, newTree) =>
    iterator(tree).toList.map { case (k, v) => if(v < parm) (k, v) else (k, v+1) } == iterator(newTree).toList
  }
}
