import collection.immutable._
import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._
import util._
import Buildable._

object Test extends Properties("TreeMap") {
  def genTreeMap[A: Arbitrary: Ordering, B: Arbitrary]: Gen[TreeMap[A, B]] =
    for {
      keys <- listOf(arbitrary[A])
      values <- listOfN(keys.size, arbitrary[B])
    } yield TreeMap(keys zip values: _*)
  implicit def arbTreeMap[A : Arbitrary : Ordering, B : Arbitrary] = Arbitrary(genTreeMap[A, B])

  property("foreach/iterator consistency") = forAll { (subject: TreeMap[Int, String]) =>
    val it = subject.iterator
    var consistent = true
    subject.foreach { element =>
      consistent &&= it.hasNext && element == it.next
    }
    consistent
  }

  property("worst-case tree height is iterable") = forAll(choose(0, 10), arbitrary[Boolean]) { (n: Int, even: Boolean) =>
    /*
     * According to "Ralf Hinze. Constructing red-black trees" [http://www.cs.ox.ac.uk/ralf.hinze/publications/#P5]
     * you can construct a skinny tree of height 2n by inserting the elements [1 .. 2^(n+1) - 2] and a tree of height
     * 2n+1 by inserting the elements [1 .. 3 * 2^n - 2], both in reverse order.
     *
     * Since we allocate a fixed size buffer in the iterator (based on the tree size) we need to ensure
     * it is big enough for these worst-case trees.
     */
    val highest = if (even) (1 << (n+1)) - 2 else 3*(1 << n) - 2
    val values = (1 to highest).reverse
    val subject = TreeMap(values zip values: _*)
    val it = subject.iterator
    try { while (it.hasNext) it.next; true } catch { case _ => false }
  }

  property("sorted") = forAll { (subject: TreeMap[Int, String]) => (subject.size >= 3) ==> {
    subject.zip(subject.tail).forall { case (x, y) => x._1 < y._1 }
  }}

  property("contains all") = forAll { (arr: List[(Int, String)]) =>
    val subject = TreeMap(arr: _*)
    arr.map(_._1).forall(subject.contains(_))
  }

  property("size") = forAll { (elements: List[(Int, Int)]) =>
    val subject = TreeMap(elements: _*)
    elements.map(_._1).distinct.size == subject.size
  }

  property("toSeq") = forAll { (elements: List[(Int, Int)]) =>
    val subject = TreeMap(elements: _*)
    elements.map(_._1).distinct.sorted == subject.toSeq.map(_._1)
  }

  property("head") = forAll { (elements: List[Int]) => elements.nonEmpty ==> {
    val subject = TreeMap(elements zip elements: _*)
    elements.min == subject.head._1
  }}

  property("last") = forAll { (elements: List[Int]) => elements.nonEmpty ==> {
    val subject = TreeMap(elements zip elements: _*)
    elements.max == subject.last._1
  }}

  property("head/tail identity") = forAll { (subject: TreeMap[Int, String]) => subject.nonEmpty ==> {
    subject == (subject.tail + subject.head)
  }}

  property("init/last identity") = forAll { (subject: TreeMap[Int, String]) => subject.nonEmpty ==> {
    subject == (subject.init + subject.last)
  }}

  property("take") = forAll { (subject: TreeMap[Int, String]) =>
    val n = choose(0, subject.size).sample.get
    n == subject.take(n).size && subject.take(n).forall(elt => subject.get(elt._1) == Some(elt._2))
  }

  property("drop") = forAll { (subject: TreeMap[Int, String]) =>
    val n = choose(0, subject.size).sample.get
    (subject.size - n) == subject.drop(n).size && subject.drop(n).forall(elt => subject.get(elt._1) == Some(elt._2))
  }

  property("take/drop identity") = forAll { (subject: TreeMap[Int, String]) =>
    val n = choose(-1, subject.size + 1).sample.get
    subject == subject.take(n) ++ subject.drop(n)
  }

  property("splitAt") = forAll { (subject: TreeMap[Int, String]) =>
    val n = choose(-1, subject.size + 1).sample.get
    val (prefix, suffix) = subject.splitAt(n)
    prefix == subject.take(n) && suffix == subject.drop(n)
  }

  def genSliceParms = for {
    tree <- genTreeMap[Int, String]
    from <- choose(0, tree.size)
    until <- choose(from, tree.size)
  } yield (tree, from, until)

  property("slice") = forAll(genSliceParms) { case (subject, from, until) =>
    val slice = subject.slice(from, until)
    slice.size == until - from && subject.toSeq == subject.take(from).toSeq ++ slice ++ subject.drop(until)
  }

  property("takeWhile") = forAll { (subject: TreeMap[Int, String]) =>
    val result = subject.takeWhile(_._1 < 0)
    result.forall(_._1 < 0) && result == subject.take(result.size)
  }

  property("dropWhile") = forAll { (subject: TreeMap[Int, String]) =>
    val result = subject.dropWhile(_._1 < 0)
    result.forall(_._1 >= 0) && result == subject.takeRight(result.size)
  }

  property("span identity") = forAll { (subject: TreeMap[Int, String]) =>
    val (prefix, suffix) = subject.span(_._1 < 0)
    prefix.forall(_._1 < 0) && suffix.forall(_._1 >= 0) && subject == prefix ++ suffix
  }

  property("from is inclusive") = forAll { (subject: TreeMap[Int, String]) => subject.nonEmpty ==> {
    val n = choose(0, subject.size - 1).sample.get
    val from = subject.drop(n).firstKey
    subject.from(from).firstKey == from && subject.from(from).forall(_._1 >= from)
  }}

  property("to is inclusive") = forAll { (subject: TreeMap[Int, String]) => subject.nonEmpty ==> {
    val n = choose(0, subject.size - 1).sample.get
    val to = subject.drop(n).firstKey
    subject.to(to).lastKey == to && subject.to(to).forall(_._1 <= to)
  }}

  property("until is exclusive") = forAll { (subject: TreeMap[Int, String]) => subject.size > 1 ==> {
    val n = choose(1, subject.size - 1).sample.get
    val until = subject.drop(n).firstKey
    subject.until(until).lastKey == subject.take(n).lastKey && subject.until(until).forall(_._1 <= until)
  }}

  property("remove single") = forAll { (subject: TreeMap[Int, String]) => subject.nonEmpty ==> {
    val key = oneOf(subject.keys.toSeq).sample.get
    val removed = subject - key
    subject.contains(key) && !removed.contains(key) && subject.size - 1 == removed.size
  }}

  property("remove all") = forAll { (subject: TreeMap[Int, String]) =>
    val result = subject.foldLeft(subject)((acc, elt) => acc - elt._1)
    result.isEmpty
  }
}
