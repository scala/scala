import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import util.logging.ConsoleLogger

package scala.collection.mutable {

  /**
   * Property of an AVL Tree : Any node of the tree has a balance value beetween in [-1; 1]
   */
  abstract class AVLTreeTest(name: String) extends Properties(name) with ConsoleLogger {

    def `2^`(n: Int) = (1 to n).fold(1)((a, b) => b*2)

    def capacityMax(depth: Int): Int = `2^`(depth+1) - 1

    def minDepthForCapacity(x: Int): Int = {
      var depth = 0
      while(capacityMax(depth) < x)
        depth += 1
      depth
    }

    def numberOfElementsInLeftSubTree(n: Int): collection.immutable.IndexedSeq[Int] = {
      val mid = n/2 + n%2
      ((1 until mid)
        .filter { i => math.abs(minDepthForCapacity(i) - minDepthForCapacity(n-i)) < 2 }
          .flatMap { i => Seq(i, n-(i+1)) }).toIndexedSeq.distinct
    }

    def makeAllBalancedTree[A](elements: List[A]): List[AVLTree[A]] = elements match {
      case Nil => Leaf::Nil
      case first::Nil => Node(first, Leaf, Leaf)::Nil
      case first::second::Nil => Node(second, Node(first, Leaf, Leaf), Leaf)::Node(first, Leaf, Node(second, Leaf, Leaf))::Nil
      case first::second::third::Nil => Node(second, Node(first, Leaf, Leaf), Node(third, Leaf, Leaf))::Nil
      case _ => {
        val combinations = for {
          left <- numberOfElementsInLeftSubTree(elements.size)
          root = elements(left)
          right = elements.size - (left + 1)
        } yield (root, left, right)
        (combinations.flatMap(triple => for {
          l <- makeAllBalancedTree(elements.take(triple._2))
          r <- makeAllBalancedTree(elements.takeRight(triple._3))
        } yield Node(triple._1, l, r))).toList
      }
    }

    def genInput: org.scalacheck.Gen[(Int, List[AVLTree[Int]])] = for {
      size <- org.scalacheck.Gen.choose(20, 25)
      elements <- org.scalacheck.Gen.listOfN(size, org.scalacheck.Gen.choose(0, 1000))
      selected <- org.scalacheck.Gen.choose(0, 1000)
    } yield {
      // selected mustn't be in elements already
      val list = makeAllBalancedTree(elements.sorted.distinct.map(_*2))
      (selected*2+1, list) 
    }

    def genInputDelete: org.scalacheck.Gen[(Int, List[AVLTree[Int]])] = for {
      size <- org.scalacheck.Gen.choose(20, 25)
      elements <- org.scalacheck.Gen.listOfN(size, org.scalacheck.Gen.choose(0, 1000))
      e = elements.sorted.distinct
      selected <- org.scalacheck.Gen.choose(0, e.size-1)
    } yield {
      // selected must be in elements already
      val list = makeAllBalancedTree(e)
      (e(selected), list) 
    }
  }

  trait AVLInvariants {
    self: AVLTreeTest =>

    def isBalanced[A](t: AVLTree[A]): Boolean = t match {
      case node: Node[A] => math.abs(node.balance) < 2 && (List(node.left, node.right) forall isBalanced)
      case Leaf => true
    }

    def setup(invariant: AVLTree[Int] => Boolean) = forAll(genInput) {
      case (selected: Int, trees: List[AVLTree[Int]]) => 
      trees.map(tree => invariant(tree)).fold(true)((a, b) => a && b)
    }

    property("Every tree is initially balanced.") = setup(isBalanced)
  }

  object TestInsert extends AVLTreeTest("Insert") with AVLInvariants {
    import math.Ordering.Int
    property("`insert` creates a new tree containing the given element. The tree remains balanced.") = forAll(genInput) {
      case (selected: Int, trees: List[AVLTree[Int]]) =>
      trees.map(tree => {
        val modifiedTree = tree.insert(selected, Int)
        modifiedTree.contains(selected, Int) && isBalanced(modifiedTree)
      }).fold(true)((a, b) => a && b)
    }
  }

  object TestRemove extends AVLTreeTest("Remove") with AVLInvariants {
    import math.Ordering.Int
    property("`remove` creates a new tree without the given element. The tree remains balanced.") = forAll(genInputDelete) {
      case (selected: Int, trees: List[AVLTree[Int]]) =>
      trees.map(tree => {
        val modifiedTree = tree.remove(selected, Int)
        tree.contains(selected, Int) && !modifiedTree.contains(selected, Int) && isBalanced(modifiedTree)
      }).fold(true)((a, b) => a && b)
    }
  }
}

object Test extends Properties("AVL") {
  include(scala.collection.mutable.TestInsert)
  include(scala.collection.mutable.TestRemove)
}
