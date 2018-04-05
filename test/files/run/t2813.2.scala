import java.util.LinkedList
import collection.convert.ImplicitConversions._

object Test extends App {
  def assertListEquals[A](expected: List[A], actual: collection.Seq[A]) {
    assert(expected.sameElements(actual),
           "Expected list to contain " + expected.mkString("[", ", ", "]") +
           ", but was " + actual.mkString("[", ", ", "]"))
  }

  def addAllOfNonCollectionWrapperAtZeroOnEmptyLinkedList() {
    val l = new LinkedList[Int]
    l.addAll(0, List(1, 2))
    assertListEquals(List(1, 2), l)
  }

  def addAllOfNonCollectionWrapperAtZeroOnLinkedList() {
    val l = new LinkedList[Int] += 1 += 2
    // `l` is a `mutable.Buffer`, which has an `addAll` method since 2.13, so the
    // implicit conversion doesn't kick in, and then there are too many arguments.
    (l: java.util.List[Int]).addAll(0, List(10, 11))
    assertListEquals((List(10, 11, 1, 2)), l)
  }

  def addAllOfCollectionWrapperAtZeroOnLinkedList() {
    val l = new LinkedList[Int] += 1 += 2
    (l: java.util.List[Int]).addAll(0, new LinkedList[Int] += 10 += 11)
    assertListEquals((List(10, 11, 1, 2)), l)
  }

  def addAllOfCollectionWrapperAtZeroOnEmptyLinkedList() {
    val l = new LinkedList[Int]
    l.addAll(0, new LinkedList[Int] += 10 += 11)
    assertListEquals((List(10, 11)), l)
  }

  addAllOfNonCollectionWrapperAtZeroOnEmptyLinkedList
  addAllOfNonCollectionWrapperAtZeroOnLinkedList
  addAllOfCollectionWrapperAtZeroOnEmptyLinkedList
  addAllOfCollectionWrapperAtZeroOnLinkedList
}
