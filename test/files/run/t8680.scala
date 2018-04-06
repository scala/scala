object Test extends App {
  def pre(n: Int) = (-n to -1).to(LazyList)

  def cyc(m: Int) = {
    lazy val s: LazyList[Int] = (0 until m).to(LazyList) #::: s
    s
  }

  def precyc(n: Int, m: Int) = pre(n) #::: cyc(m)

  def str(s: LazyList[Int]) = {
    val b = new StringBuilder
    s.addString(b, "", "", "")
    b.toString
  }

  def goal(n: Int, m: Int) = (-n until m).mkString + "..."

  /* TODO-newColl: re-enable once https://github.com/scala/collection-strawman/issues/550 is fixed
  // Check un-forced cyclic and non-cyclic streams
  assert(str(pre(2)) == pre(2).take(1).toList.mkString + "?")
  assert(str(cyc(2)) == cyc(2).take(1).toList.mkString + "?")
  assert(str(precyc(2,2)) == precyc(2,2).take(1).toList.mkString + "?")
  assert(pre(2).knownSize < 0)
  assert(cyc(2).knownSize < 0)
  assert(precyc(2,2).knownSize < 0)

  // Check forced cyclic and non-cyclic streams
  assert(str(pre(2).force) == (-2 to -1).mkString)
  assert(str(cyc(2).force) == (0 until 2).mkString + "...")
  assert(str(precyc(2,2).force) == (-2 until 2).mkString + "...")
  assert(pre(2).force.knownSize >= 0)
  assert(cyc(2).force.knownSize < 0)
  assert(precyc(2,2).force.knownSize < 0)

  // Special cases
  assert(str(cyc(1).force) == goal(0,1))
  assert(str(precyc(1,6).force) == goal(1,6))
  assert(str(precyc(6,1).force) == goal(6,1))

  // Make sure there are no odd/even problems
  for (n <- 3 to 4; m <- 3 to 4) {
    assert(precyc(n,m).mkString == goal(n,m), s"mkString $n $m")
    assert(precyc(n,m).force.knownSize < 0, s"hasDef $n$m")
  }

  // Make sure there are no cycle/prefix modulus problems
  for (i <- 6 to 8) {
    assert(precyc(i,3).mkString == goal(i,3), s"mkString $i 3")
    assert(precyc(3,i).mkString == goal(3,i), s"mkString 3 $i")
    assert(precyc(i,3).force.knownSize < 0, s"hasDef $i 3")
    assert(precyc(3,i).force.knownSize < 0, s"hasDef 3 $i")
  }
  */
}
