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
  object RedBlackTest extends scala.collection.immutable.RedBlack[Int] {
    def isSmaller(x: Int, y: Int) = x < y
  }

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
        blackNodesToLeaves(ne).removeDuplicates.size == 1
        && areBlackNodesToLeavesEqual(ne.left)
        && areBlackNodesToLeavesEqual(ne.right)
      )
  }

  def orderIsPreserved[A](t: Tree[A]): Boolean = t match {
    case Empty => true
    case ne: NonEmpty[_] =>
      (
        (ne.left.iterator map (_._1) forall (isSmaller(_, ne.key)))
        && (ne.right.iterator map (_._1) forall (isSmaller(ne.key, _)))
        && (List(ne.left, ne.right) forall orderIsPreserved)
      )
  }

  def setup(l: List[Int], invariant: Tree[Unit] => Boolean): (Boolean, Tree[Unit])

  def listNoRepetitions(size: Int) = for {
    s <- Gen.choose(1, size)
    l <- Gen.listOfN(size, Gen.choose(0, Int.MaxValue)) suchThat (l => l.size == l.removeDuplicates.size)
  } yield l
  def listFewRepetitions(size: Int) = for {
    s <- Gen.choose(1, size)
    l <- Gen.listOfN(s, Gen.choose(0, size * 4)) suchThat (l => l.size != l.removeDuplicates.size)
  } yield l
  def listManyRepetitions(size: Int) =  for {
    s <- Gen.choose(1, size)
    l <- Gen.listOfN(s, Gen.choose(0, size)) suchThat (l => l.size != l.removeDuplicates.size)
  } yield l
  def listEvenRepetitions(size: Int) = listFewRepetitions(size) map (x =>
    scala.util.Random.shuffle(x zip x flatMap { case (a, b) => List(a, b) })
  )

  // Arbitrarily weighted list distribution types
  val seqType: Gen[Int => Gen[List[Int]]]

  def myGen(sized: Int) = for {
    size <- Gen.choose(0, sized)
    seq <- seqType
    list <- seq(size)
  } yield list

  property("root is black") = forAll(myGen(10)) { l =>
    setup(l, rootIsBlack)._1 :| setup(l, rootIsBlack)._2.toString
  }
  property("all leaves are black") = forAll(myGen(50)) { l =>
    setup(l, areAllLeavesBlack)._1 :| setup(l, areAllLeavesBlack)._2.toString
  }
  property("children of red nodes are black") = forAll(myGen(50)) { l =>
    setup(l, areRedNodeChildrenBlack)._1 :| setup(l, areRedNodeChildrenBlack)._2.toString
  }
  property("Every path from a node to its descendant leaves contains the same number of black nodes") = forAll(myGen(50)) { l =>
    setup(l, areBlackNodesToLeavesEqual)._1 :| setup(l, areBlackNodesToLeavesEqual)._2.toString
  }
  property("Ordering of keys is preserved") = forAll(myGen(50)) { l =>
    setup(l, orderIsPreserved)._1 :| setup(l, orderIsPreserved)._2.toString
  }
}

object TestInsertion extends RedBlackTest {
  import RedBlackTest._
  override val seqType = Gen.frequency(
    (1, listNoRepetitions _),
    (1, listManyRepetitions _)
  )

  property("update adds elements") = forAll(myGen(50)) { l =>
    val tree = l.foldLeft(Empty: Tree[Unit])((acc, n) => acc update (n, ()))
    forAll(Gen.pick(1, l)) ( n => !(tree lookup n.head isEmpty) :| "Tree: "+tree+" N: "+n.head )
  }

  override def setup(l: List[Int], invariant: Tree[Unit] => Boolean) = l.foldLeft((true, Empty: Tree[Unit])) {
    case ((true, acc), n) =>
      val newRoot = acc update (n, ())
      (invariant(newRoot), newRoot)
    case (failed, _) => failed
  }
}

object TestDeletion extends RedBlackTest {
  import RedBlackTest._
  override val seqType = Gen.frequency(
    (2, listFewRepetitions _),
    (3, listManyRepetitions _),
    (1, listEvenRepetitions _)
  )

  property("delete removes elements") = forAll(myGen(50)) { l =>
    val tree = l.foldLeft(Empty: Tree[Unit])((acc, n) => acc update (n, ()))
    forAll(Gen.choose(1, l.size)) { numberOfElementsToRemove =>
      forAll(Gen.pick(numberOfElementsToRemove, l)) { elementsToRemove =>
        val newTree = elementsToRemove.foldLeft(tree)((acc, n) => acc delete n)
        (elementsToRemove forall (n => newTree lookup n isEmpty)) :| "Tree: "+tree+"New Tree: "+newTree+" Elements to Remove: "+elementsToRemove
      }
    }
  }

  override def setup(l: List[Int], invariant: Tree[Unit] => Boolean) = l.foldLeft((true, Empty: Tree[Unit])) {
    case ((true, acc), n) =>
      val newRoot = if (acc lookup n isEmpty) acc update (n, ()) else acc delete n
      (invariant(newRoot), newRoot)
    case (failed, _) => failed
  }
}

object Test extends Properties("RedBlack") {
  include(TestInsertion)
  include(TestDeletion)
}

