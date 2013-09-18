



import collection.mutable.UnrolledBuffer



object Test {

  def main(args: Array[String]) {
    val u1 = new UnrolledBuffer[Int]
    assert(u1.isEmpty)
    assert(u1.size == 0)

    u1 += 1
    u1 += 2
    u1 += 3
    assert(u1 == UnrolledBuffer(1, 2, 3))
    assert(u1.toList == List(1, 2, 3))
    assert(u1.nonEmpty)
    assert(u1.size == 3)

    u1.clear
    assert(u1.isEmpty)
    assert(u1.size == 0)

    u1 += 1
    u1 += 2
    u1 += 3
    u1.remove(1)
    assert(u1.nonEmpty)
    assert(u1.size == 2)
    assert(u1 == UnrolledBuffer(1, 3))
    assert(u1.toList == List(1, 3))

    u1 concat UnrolledBuffer(5, 7, 9)
    assert(u1 == UnrolledBuffer(1, 3, 5, 7, 9))

    val u2 = u1 map { x => (x - 1) / 2 }
    assert(u2 == UnrolledBuffer(0, 1, 2, 3, 4))

    u1.clear
    u2.clear
    assert(u1.size == 0)
    assert(u2.size == 0)

    for (i <- 0 until 500) u1 += i
    for (i <- 500 until 1000) u2 += i
    assert(u1.size == 500)
    assert(u2.size == 500)
    assert(u1.iterator.toList == (0 until 500).toList)
    assert((for (elem <- u1) yield elem) sameElements (0 until 500))

    u1 concat u2
    assert(u1.size == 1000)
    assert(u2.size == 0)
    assertCorrect(u1)

    u1 concat UnrolledBuffer()
    assertCorrect(u1)

    val u3 = u1 map { x => x }
    var i = 0
    for (elem <- u1) {
      assert(elem == u3(i))
      i += 1
    }

    u1.remove(999)
    assert(u1.size == 999)
    assertCorrect(u1)

    u1.remove(500)
    assert(u1.size == 998)
    assertCorrect(u1)

    u1.remove(5)
    assert(u1.size == 997)
    assertCorrect(u1)

    u1.remove(0)
    assert(u1.size == 996)
    assertCorrect(u1)

    u1.insert(0, 0)
    assert(u1.size == 997)
    assertCorrect(u1)

    u1.insert(5, 5)
    assert(u1.size == 998)
    assertCorrect(u1)

    u1.insert(500, 500)
    assert(u1.size == 999)
    assertCorrect(u1)

    u1.insert(999, 999)
    assert(u1.size == 1000)
    assertCorrect(u1)

    for (i <- -100 until 0) {
      i +=: u1
      assertCorrect(u1)
    }
    assert(u1.size == 1100)
    assertCorrect(u1)
  }

  def assertCorrect(u1: UnrolledBuffer[Int]) {
    val sz = u1.size
    val store = new Array[Int](sz)
    for (i <- 0 until sz) {
      store(i) = u1(i)
      u1(i) = sz - i
    }
    for (i <- 0 until sz) assert(u1(i) == (sz - i))
    for (i <- 0 until sz) u1(i) = store(i)
    for (i <- 0 until sz) assert(store(i) == u1(i))

    assert((u1 map { x => x }) == u1)
    assert(u1.iterator.toSeq.size == u1.size)
  }

}
