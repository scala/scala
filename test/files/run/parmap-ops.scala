import collection._

object Test {

  def main(args: Array[String]) {
    val gm: GenMap[Int, Int] = GenMap(0 -> 0, 1 -> 1).par

    // ops
    assert(gm.isDefinedAt(1))
    assert(gm.contains(1))
    assert(gm.getOrElse(1, 2) == 1)
    assert(gm.getOrElse(2, 3) == 3)
    assert(gm.keysIterator.toSet == Set(0, 1))
    assert(gm.valuesIterator.toSet == Set(0, 1))
    assert(gm.keySet == Set(0, 1))
    assert(gm.keys.toSet == Set(0, 1))
    assert(gm.values.toSet == Set(0, 1))
    try {
      gm.default(-1)
      assert(false)
    } catch {
      case e: NoSuchElementException => // ok
    }

    assert(gm.filterKeys(_ % 2 == 0)(0) == 0)
    assert(gm.filterKeys(_ % 2 == 0).get(1) == None)
    assert(gm.mapValues(_ + 1)(0) == 1)

    // with defaults
    val pm = parallel.mutable.ParMap(0 -> 0, 1 -> 1)
    val dm = pm.withDefault(x => -x)
    assert(dm(0) == 0)
    assert(dm(1) == 1)
    assert(dm(2) == -2)
    assert(dm.updated(2, 2) == parallel.ParMap(0 -> 0, 1 -> 1, 2 -> 2))
    dm.put(3, 3)
    assert(dm(3) == 3)
    assert(pm(3) == 3)
    assert(dm(4) == -4)

    val imdm = parallel.immutable.ParMap(0 -> 0, 1 -> 1).withDefault(x => -x)
    assert(imdm(0) == 0)
    assert(imdm(1) == 1)
    assert(imdm(2) == -2)
    assert(imdm.updated(2, 2) == parallel.ParMap(0 -> 0, 1 -> 1, 2 -> 2))
  }

}
