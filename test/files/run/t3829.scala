// ticket #3829
object Test {
  import collection.{ mutable, immutable }

  def main(args: Array[String]) {
    val map = immutable.Map(1 -> 2, 3 -> 4)
    assert(map.get(0) == None)

    // Since r24255 defaultMap.get(x) returns None rather than
    // using the default, so these mostly use apply.
    val defmap = map.withDefaultValue(-1)
    assert(defmap(0) == -1)
    assert(defmap.size == 2)
    assert(defmap.iterator.size == 2)
    assert(defmap.empty(0) == -1)
    assert((defmap + (2 -> 3))(0) == -1)
    assert((defmap + (2 -> 3))(1) == 2)
    assert((defmap + (2 -> 3))(2) == 3)
    assert((defmap - 1)(0) == -1)
    assert((defmap - 1)(1) == -1)
    assert((defmap - 1)(3) == 4)

    val mutmap = mutable.Map(1 -> 2, 2 -> 3)
    assert(mutmap.get(0) == None)

    val defmutmap = mutmap.withDefaultValue(-1)
    assert(defmutmap(0) == -1)
    assert(defmutmap(3) == -1)
    mutmap += 3 -> 4
    assert(defmutmap(3) == 4)
    assert(defmutmap(1) == 2)
    mutmap -= 1
    assert(defmutmap(1) == -1)
    assert(mutmap.get(1) == None)
    defmutmap += 1 -> 2
    assert(defmutmap(1) == 2)
    assert(mutmap(1) == 2)
  }

}
