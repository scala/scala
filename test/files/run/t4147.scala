


import scala.collection._



object Test {

  def main(args: Array[String]) {
    checkElementsAreSorted()
    checkRangedImpl()
  }

  def checkElementsAreSorted() {
    val tree = mutable.SortedSet[Int]()
    tree ++= List(4, 3, 1, 6, 7, 5, 2)
    assert(tree == immutable.SortedSet(1, 2, 3, 4, 5, 6, 7))
    assert(tree.size == 7)
  }

  def checkRangedImpl() {
    val tree = mutable.SortedSet[Int](3, 1, 6, 7, 5, 2)
    val projection = tree.rangeImpl(Some(3), Some(6))
    assert(projection == immutable.SortedSet(3, 5))
    assert(projection.size == 2)

    // Let's check that modification are taken into account
    tree add 4
    assert(tree == immutable.SortedSet(1, 2, 3, 4, 5, 6, 7))
    assert(projection == immutable.SortedSet(3, 4, 5))
    assert(tree.size == 7)
    assert(projection.size == 3)
  }

}
