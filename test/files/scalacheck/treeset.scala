import collection.immutable._
import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._
import util._

object Test extends Properties("TreeSet") {
  def genTreeSet[A: Arbitrary: Ordering]: Gen[TreeSet[A]] =
    for {
      elements <- listOf(arbitrary[A])
    } yield TreeSet(elements: _*)
  implicit def arbTreeSet[A : Arbitrary : Ordering]: Arbitrary[TreeSet[A]] = Arbitrary(genTreeSet)

  property("foreach/iterator consistency") = forAll { (subject: TreeSet[Int]) =>
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
    val subject = TreeSet(values: _*)
    val it = subject.iterator
    try { while (it.hasNext) it.next; true } catch { case _ => false }
  }

  property("sorted") = forAll { (subject: TreeSet[Int]) => (subject.size >= 3) ==> {
    subject.zip(subject.tail).forall { case (x, y) => x < y }
  }}

  property("contains all") = forAll { (elements: List[Int]) =>
    val subject = TreeSet(elements: _*)
    elements.forall(subject.contains)
  }

  property("size") = forAll { (elements: List[Int]) =>
    val subject = TreeSet(elements: _*)
    elements.distinct.size == subject.size
  }

  property("toSeq") = forAll { (elements: List[Int]) =>
    val subject = TreeSet(elements: _*)
    elements.distinct.sorted == subject.toSeq
  }

  property("head") = forAll { (elements: List[Int]) => elements.nonEmpty ==> {
    val subject = TreeSet(elements: _*)
    elements.min == subject.head
  }}

  property("last") = forAll { (elements: List[Int]) => elements.nonEmpty ==> {
    val subject = TreeSet(elements: _*)
    elements.max == subject.last
  }}

  property("head/tail identity") = forAll { (subject: TreeSet[Int]) => subject.nonEmpty ==> {
    subject == (subject.tail + subject.head)
  }}

  property("init/last identity") = forAll { (subject: TreeSet[Int]) => subject.nonEmpty ==> {
    subject == (subject.init + subject.last)
  }}

  property("take") = forAll { (subject: TreeSet[Int]) =>
    val n = choose(0, subject.size).sample.get
    n == subject.take(n).size && subject.take(n).forall(subject.contains)
  }

  property("drop") = forAll { (subject: TreeSet[Int]) =>
    val n = choose(0, subject.size).sample.get
    (subject.size - n) == subject.drop(n).size && subject.drop(n).forall(subject.contains)
  }

  property("take/drop identity") = forAll { (subject: TreeSet[Int]) =>
    val n = choose(-1, subject.size + 1).sample.get
    subject == subject.take(n) ++ subject.drop(n)
  }

  property("splitAt") = forAll { (subject: TreeSet[Int]) =>
    val n = choose(-1, subject.size + 1).sample.get
    val (prefix, suffix) = subject.splitAt(n)
    prefix == subject.take(n) && suffix == subject.drop(n)
  }

  def genSliceParms = for {
    tree <- genTreeSet[Int]
    from <- choose(0, tree.size)
    until <- choose(from, tree.size)
  } yield (tree, from, until)

  property("slice") = forAll(genSliceParms) { case (subject, from, until) =>
    val slice = subject.slice(from, until)
    slice.size == until - from && subject.toSeq == subject.take(from).toSeq ++ slice ++ subject.drop(until)
  }

  property("takeWhile") = forAll { (subject: TreeSet[Int]) =>
    val result = subject.takeWhile(_ < 0)
    result.forall(_ < 0) && result == subject.take(result.size)
  }

  property("dropWhile") = forAll { (subject: TreeSet[Int]) =>
    val result = subject.dropWhile(_ < 0)
    result.forall(_ >= 0) && result == subject.takeRight(result.size)
  }

  property("span identity") = forAll { (subject: TreeSet[Int]) =>
    val (prefix, suffix) = subject.span(_ < 0)
    prefix.forall(_ < 0) && suffix.forall(_ >= 0) && subject == prefix ++ suffix
  }

  property("from is inclusive") = forAll { (subject: TreeSet[Int]) => subject.nonEmpty ==> {
    val n = choose(0, subject.size - 1).sample.get
    val from = subject.drop(n).firstKey
    subject.from(from).firstKey == from && subject.from(from).forall(_ >= from)
  }}

  property("to is inclusive") = forAll { (subject: TreeSet[Int]) => subject.nonEmpty ==> {
    val n = choose(0, subject.size - 1).sample.get
    val to = subject.drop(n).firstKey
    subject.to(to).lastKey == to && subject.to(to).forall(_ <= to)
  }}

  property("until is exclusive") = forAll { (subject: TreeSet[Int]) => subject.size > 1 ==> {
    val n = choose(1, subject.size - 1).sample.get
    val until = subject.drop(n).firstKey
    subject.until(until).lastKey == subject.take(n).lastKey && subject.until(until).forall(_ <= until)
  }}

  property("remove single") = forAll { (subject: TreeSet[Int]) => subject.nonEmpty ==> {
    val element = oneOf(subject.toSeq).sample.get
    val removed = subject - element
    subject.contains(element) && !removed.contains(element) && subject.size - 1 == removed.size
  }}

  property("remove all") = forAll { (subject: TreeSet[Int]) =>
    val result = subject.foldLeft(subject)((acc, elt) => acc - elt)
    result.isEmpty
  }

  property("ordering must not be null") =
    throws(classOf[NullPointerException])(TreeSet.empty[Int](null))
}
