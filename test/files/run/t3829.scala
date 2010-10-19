// ticket #3829
object Test {
  import collection.{ mutable, immutable }

  def main(args: Array[String]) {
    val map = immutable.Map(1 -> 2, 3 -> 4)
    assert(map.get(0) == None)

    val defmap = map.withDefaultValue(-1)
    assert(defmap.get(0) == Some(-1))
    assert(defmap.size == 2)
    assert(defmap.iterator.size == 2)
    assert(defmap.empty.get(0) == Some(-1))
    assert((defmap + (2 -> 3)).get(0) == Some(-1))
    assert((defmap + (2 -> 3)).get(1) == Some(2))
    assert((defmap + (2 -> 3)).get(2) == Some(3))
    assert((defmap - 1).get(0) == Some(-1))
    assert((defmap - 1).get(1) == Some(-1))
    assert((defmap - 1).get(3) == Some(4))

    val mutmap = mutable.Map(1 -> 2, 2 -> 3)
    assert(mutmap.get(0) == None)

    val defmutmap = mutmap.withDefaultValue(-1)
    assert(defmutmap.get(0) == Some(-1))
    assert(defmutmap.get(3) == Some(-1))
    mutmap += 3 -> 4
    assert(defmutmap.get(3) == Some(4))
    assert(defmutmap.get(1) == Some(2))
    mutmap -= 1
    assert(defmutmap.get(1) == Some(-1))
    assert(mutmap.get(1) == None)
    defmutmap += 1 -> 2
    assert(defmutmap.get(1) == Some(2))
    assert(mutmap.get(1) == Some(2))
  }

}
