/**
 * Checks that various foreach methods overridden in mutable.HashMap as part of ticket #2526
 * still work correctly.
 */
object Test {
  import collection._

  def main(args: Array[String]) {
    val m = new mutable.HashMap[String, String]

    /* Use non hash-based structure for verification */
    val keys = List("a", "b", "c", "d", "e")
    val valueSuffix = "value"
    val values = keys.map(_ + valueSuffix)
    val entries = keys.zip(values)

    for (k <- keys) m(k) = k + valueSuffix

    assertForeach(keys, m.keySet.iterator)
    assertForeach(keys, m.keysIterator)
    assertForeach(keys, m.keySet)

    assertForeach(values, m.values.iterator)
    assertForeach(values, m.valuesIterator)

    assertForeach(entries, m)
  }

  /* Checks foreach of `actual` goes over all the elements in `expected` */
  private def assertForeach[E](expected: Traversable[E], actual: Iterator[E]): Unit = {
    val notYetFound = new mutable.ArrayBuffer[E]() ++= expected
    actual.foreach { e =>
      assert(notYetFound.contains(e))
      notYetFound -= e
    }
    assert(notYetFound.size == 0, "mutable.HashMap.foreach should have iterated over: " + notYetFound)
  }

  /*
   * Checks foreach of `actual` goes over all the elements in `expected`
   * We duplicate the method above because there is no common interface between Traversable and
   * Iterator and we want to avoid converting between collections to ensure that we test what
   * we mean to test.
   */
  private def assertForeach[E](expected: Traversable[E], actual: Traversable[E]): Unit = {
    val notYetFound = new mutable.ArrayBuffer[E]() ++= expected
    actual.foreach { e =>
      assert(notYetFound.contains(e))
      notYetFound -= e
    }
    assert(notYetFound.size == 0, "mutable.HashMap.foreach should have iterated over: " + notYetFound)
  }
}
