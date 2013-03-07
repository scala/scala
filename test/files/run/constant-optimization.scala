object Test extends App {
  def testBothReachable() {
    val i = util.Random.nextInt
    val x = if (i % 2 == 0) null else "good"
    val y = if (x == null) "good" else x + ""
    println(s"testBothReachable: $y")
  }

  def testOneReachable() {
    val i = 1
    val x = if (i != 1) null else "good"
    val y = if (x == null) "good" else x + ""
    println(s"testOneReachable: $y")
  }

  testBothReachable()
  testOneReachable()
}
