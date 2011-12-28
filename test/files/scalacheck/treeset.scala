import collection.immutable._
import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._
import util._

object Test extends Properties("TreeSet") {
  implicit def arbTreeSet[A : Arbitrary : Ordering]: Arbitrary[TreeSet[A]] =
    Arbitrary(listOf(arbitrary[A]) map (elements => TreeSet(elements: _*)))

  property("foreach/iterator consistency") = forAll { (subject: TreeSet[Int]) =>
    val it = subject.iterator
    var consistent = true
    subject.foreach { element =>
      consistent &&= it.hasNext && element == it.next
    }
    consistent
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

  property("remove single") = forAll { (subject: TreeSet[Int]) => subject.nonEmpty ==> {
    val element = oneOf(subject.toSeq).sample.get
    val removed = subject - element
    subject.contains(element) && !removed.contains(element) && subject.size - 1 == removed.size
  }}

  property("remove all") = forAll { (subject: TreeSet[Int]) =>
    val result = subject.foldLeft(subject)((acc, elt) => acc - elt)
    result.isEmpty
  }
}
