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

  def testAllReachable() {
    val i = util.Random.nextInt
    val y = (i % 2) match {
      case 0 => "good"
      case 1 => "good"
      case _ => "good"
    }
    println(s"testAllReachable: $y")
  }

  def testOneUnreachable() {
    val i = util.Random.nextInt
    val x = if (i % 2 == 0) {
      1
    } else {
      2
    }
    val y = x match {
      case 0 => "good"
      case 1 => "good"
      case _ => "good"
    }
    println(s"testOneUnreachable: $y")
  }

  def testDefaultUnreachable() {
    val i = util.Random.nextInt
    val x = if (i % 2 == 0) {
      1
    } else {
      2
    }
    val y = x match {
      case 1 => "good"
      case 2 => "good"
      case _ => "good"
    }
    println(s"testDefaultUnreachable: $y")
  }

  testBothReachable()
  testOneReachable()
  testAllReachable()
  testOneUnreachable()
  testDefaultUnreachable()
}
