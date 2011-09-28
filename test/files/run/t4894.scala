




object Test {

  def main(args: Array[String]) {
    import collection._
    val hs = mutable.HashSet[Int]()
    hs ++= 1 to 10
    hs --= 1 to 10

    val phs = parallel.mutable.ParHashSet[Int]()
    phs ++= 1 to 10
    for (i <- 1 to 10) assert(phs(i))
    phs --= 1 to 10
    assert(phs.isEmpty)

    val phm = parallel.mutable.ParHashMap[Int, Int]()
    phm ++= ((1 to 10) zip (1 to 10))
    for (i <- 1 to 10) assert(phm(i) == i)
    phm --= 1 to 10
    assert(phm.isEmpty)
  }

}
