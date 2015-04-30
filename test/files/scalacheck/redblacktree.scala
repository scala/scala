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

package scala.collection.immutable.redblacktree {
  abstract class RedBlackTreeTest extends Properties("RedBlackTree") {
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
  }

  trait RedBlackTreeInvariants {
    self: RedBlackTreeTest =>

    import RB._

    def rootIsBlack[A](t: Tree[String, A]) = isBlack(t)

    def areAllLeavesBlack[A](t: Tree[String, A]): Boolean = t match {
      case null => isBlack(t)
      case ne => List(ne.left, ne.right) forall areAllLeavesBlack
    }

    def areRedNodeChildrenBlack[A](t: Tree[String, A]): Boolean = t match {
      case RedTree(_, _, left, right) => List(left, right) forall (t => isBlack(t) && areRedNodeChildrenBlack(t))
      case BlackTree(_, _, left, right) => List(left, right) forall areRedNodeChildrenBlack
      case null => true
    }

    def blackNodesToLeaves[A](t: Tree[String, A]): List[Int] = t match {
      case null => List(1)
      case BlackTree(_, _, left, right) => List(left, right) flatMap blackNodesToLeaves map (_ + 1)
      case RedTree(_, _, left, right) => List(left, right) flatMap blackNodesToLeaves
    }

    def areBlackNodesToLeavesEqual[A](t: Tree[String, A]): Boolean = t match {
      case null => true
      case ne =>
        (
          blackNodesToLeaves(ne).distinct.size == 1
          && areBlackNodesToLeavesEqual(ne.left)
          && areBlackNodesToLeavesEqual(ne.right)
        )
    }

    def orderIsPreserved[A](t: Tree[String, A]): Boolean =
      iterator(t) zip iterator(t).drop(1) forall { case (x, y) => x._1 < y._1 }

    def heightIsBounded(t: Tree[_, _]): Boolean = height(t) <= (2 * (32 - Integer.numberOfLeadingZeros(count(t) + 2)) - 2)

    def setup(invariant: Tree[String, Int] => Boolean) = forAll(genInput) { case (tree, parm, newTree) =>
      invariant(newTree)
    }

    property("root is black") = setup(rootIsBlack)
    property("all leaves are black") = setup(areAllLeavesBlack)
    property("children of red nodes are black") = setup(areRedNodeChildrenBlack)
    property("black nodes are balanced") = setup(areBlackNodesToLeavesEqual)
    property("ordering of keys is preserved") = setup(orderIsPreserved)
    property("height is bounded") = setup(heightIsBounded)
  }

  object TestInsert extends RedBlackTreeTest with RedBlackTreeInvariants {
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

  object TestModify extends RedBlackTreeTest {
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

  object TestDelete extends RedBlackTreeTest with RedBlackTreeInvariants  {
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

  object TestRange extends RedBlackTreeTest with RedBlackTreeInvariants  {
    import RB._

    override type ModifyParm = (Option[Int], Option[Int])
    override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = for {
      from <- choose(0, iterator(tree).size)
      to <- choose(0, iterator(tree).size) suchThat (from <=)
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
        .filter(key => from forall (key >=))
        .filter(key => to forall (key <))
        .toList)
      filteredTree == keysIterator(newTree).toList
    }
  }

  object TestDrop extends RedBlackTreeTest with RedBlackTreeInvariants  {
    import RB._

    override type ModifyParm = Int
    override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, iterator(tree).size)
    override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = drop(tree, parm)

    property("drop") = forAll(genInput) { case (tree, parm, newTree) =>
      iterator(tree).drop(parm).toList == iterator(newTree).toList
    }
  }

  object TestTake extends RedBlackTreeTest with RedBlackTreeInvariants  {
    import RB._

    override type ModifyParm = Int
    override def genParm(tree: Tree[String, Int]): Gen[ModifyParm] = choose(0, iterator(tree).size)
    override def modify(tree: Tree[String, Int], parm: ModifyParm): Tree[String, Int] = take(tree, parm)

    property("take") = forAll(genInput) { case (tree, parm, newTree) =>
      iterator(tree).take(parm).toList == iterator(newTree).toList
    }
  }

  object TestSlice extends RedBlackTreeTest with RedBlackTreeInvariants  {
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
}

object Test extends Properties("RedBlackTree") {
  import collection.immutable.redblacktree._
  include(TestInsert)
  include(TestModify)
  include(TestDelete)
  include(TestRange)
  include(TestDrop)
  include(TestTake)
  include(TestSlice)
}
