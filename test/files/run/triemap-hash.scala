


import util.hashing.Hashing



object Test {

  def main(args: Array[String]) {
    hashing()
    equality()
  }

  def hashing() {
    import collection._

    val tm = new concurrent.TrieMap[String, String](Hashing.fromFunction(x => x.length + x(0).toInt), Equiv.universal)
    tm.put("a", "b")
    tm.put("c", "d")

    assert(tm("a") == "b")
    assert(tm("c") == "d")

    for (i <- 0 until 1000) tm(i.toString) = i.toString
    for (i <- 0 until 1000) assert(tm(i.toString) == i.toString)
  }

  def equality() {
    import collection._

    val tm = new concurrent.TrieMap[String, String](Hashing.fromFunction(x => x(0).toInt), Equiv.fromFunction(_(0) == _(0)))
    tm.put("a", "b")
    tm.put("a1", "d")
    tm.put("b", "c")

    assert(tm("a") == "d", tm)
    assert(tm("b") == "c", tm)

    for (i <- 0 until 1000) tm(i.toString) = i.toString
    assert(tm.size == 12, tm)
    assert(tm("0") == "0", tm)
    for (i <- 1 to 9) assert(tm(i.toString) == i.toString + "99", tm)
  }

}
