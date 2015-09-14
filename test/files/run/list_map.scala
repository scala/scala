import collection.immutable.ListMap

object Test {
  def testImmutableMinus() {
    val empty = ListMap.empty[Int, Int]

    val m0 = ListMap(1 -> 1, 2 -> 2)
    val m1 = m0 - 3
    assert (m1 eq m0)
    val m2 = m0 - 1
    assert (m2.size == 1)
    val m3 = m2 - 2
    assert (m3 eq empty)

    val m4 = ListMap(1 -> 1, 2 -> 2, 3 -> 3)
    val m5 = m4 - 1
    assert (m5 == ListMap(2 -> 2, 3 -> 3))
    assert (m5.toList == (2, 2)::(3, 3)::Nil)

    assert ((empty - 1) eq empty)
  }

  def main(args: Array[String]) {
    testImmutableMinus()
  }
}
