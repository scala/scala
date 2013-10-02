

import collection._


object Test {

  def main(args: Array[String]) {
    val m = scala.collection.mutable.LinkedHashMap("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5)
    val expected = List("one", "two", "three", "four", "five")
    assert(m.keys.iterator.toList == expected)
    assert(m.keys.drop(0).iterator.toList == expected)
    assert(m.keys.drop(1).iterator.toList == expected.drop(1))
    assert(m.keys.drop(2).iterator.toList == expected.drop(2))
    assert(m.keys.drop(3).iterator.toList == expected.drop(3))
    assert(m.keys.drop(4).iterator.toList == expected.drop(4))
    assert(m.keys.drop(5).iterator.toList == expected.drop(5))

    val expvals = List(1, 2, 3, 4, 5)
    assert(m.values.iterator.toList == expvals)
    assert(m.values.drop(0).iterator.toList == expvals)
    assert(m.values.drop(1).iterator.toList == expvals.drop(1))
    assert(m.values.drop(2).iterator.toList == expvals.drop(2))
    assert(m.values.drop(3).iterator.toList == expvals.drop(3))
    assert(m.values.drop(4).iterator.toList == expvals.drop(4))
    assert(m.values.drop(5).iterator.toList == expvals.drop(5))

    val pred = (x: String) => x.length < 6
    val filtered = m.filterKeys(pred)
    assert(filtered.drop(0).keys.toList == expected.filter(pred))
    assert(filtered.drop(1).keys.toList == expected.filter(pred).drop(1))
    assert(filtered.drop(2).keys.toList == expected.filter(pred).drop(2))
    assert(filtered.drop(3).keys.toList == expected.filter(pred).drop(3))
    assert(filtered.drop(4).keys.toList == expected.filter(pred).drop(4))

    val mapped = m.mapValues(-_)
    assert(mapped.drop(0).keys.toList == expected)
    assert(mapped.drop(1).keys.toList == expected.drop(1))
    assert(mapped.drop(2).keys.toList == expected.drop(2))
    assert(mapped.drop(3).keys.toList == expected.drop(3))
    assert(mapped.drop(4).keys.toList == expected.drop(4))
    assert(mapped.drop(5).keys.toList == expected.drop(5))
  }

}
