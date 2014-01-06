object Test {
  val LIMIT = 10

  def test1 {
    var d = 2
    var i = 0 // avoid infinite loops
    while (d < LIMIT && i < LIMIT) {
      lazy val b = d + 1
      d = b
      i += 1
      println("test1: " + d)
    }
  }

  def test2 {
    var d = 2
    var i = 0
    while (true) {
      lazy val b = d + 1
      d = b
      i += 1
      println("test2: " + d)

      if (d >= LIMIT || i >= LIMIT)
        return
    }
  }

  def test3 {
    var d = 2
    var i = 0
    do {
      lazy val b = d + 1
      d = b
      i += 1
      println("test3: " + d)
    } while (d < LIMIT && i < LIMIT)
  }

  def test4 {
    var d = 2
    var i = 0
    do {
      lazy val b = d + 1
      d = b
      i += 1
      println("test4: " + d)
      if (d >= LIMIT || i >= LIMIT)
        return
    } while (true)
  }

  def test5 {
    var d = 2
    var i = 0
    while (d < LIMIT && i < LIMIT) {
      lazy val b = d + 1
      d = b
      i += 1
      println("test5.1: " + d)

      var e = 2
      var j = 0
      while (e < LIMIT && j < LIMIT) {
        lazy val f = e + 1
        e = f
        j += 1
        println("test5.2: " + e)
      }
    }
  }


  def main(args: Array[String]) {
    test1
    test2
    test3
    test4
    test5
  }
}
