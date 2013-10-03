// #2417
object Test {

  def parallel(numThreads: Int)(block: => Unit) {
    var failure: Throwable = null
    val threads = Array.tabulate(numThreads)(i => new Thread {
      override def run {
        try {
          block
          } catch {
            case x: Throwable => failure = x
          }
        }
      })
      for (t <- threads) t.start
      for (t <- threads) t.join
      if (failure != null) println("FAILURE: " + failure)
    }

    def testSet(initialSize: Int, numThreads: Int, passes: Int) {
      val orig = Set.empty ++ (1 to initialSize)
      parallel(numThreads) {
        for (pass <- 0 until passes) {
          var s = orig
          for (e <- (initialSize to 1 by -1)) {
            s -= e
            val obs = s.size
            if (obs != e - 1) {
              throw new Exception("removed e=" + e + ", size was " + obs + ", s=" + s)
            }
          }
        }
      }
    }

    def testMap(initialSize: Int, numThreads: Int, passes: Int) {
      val orig = Map.empty ++ ((1 to initialSize) map ((_,"v")))
      parallel(numThreads) {
        for (pass <- 0 until passes) {
          var m = orig
          for (e <- (initialSize to 1 by -1)) {
            m -= e
            val obs = m.size
            if (obs != e - 1) {
              throw new Exception("removed e=" + e + ", size was " + obs + ", m=" + m)
            }
          }
        }
      }
    }

    def main(args: Array[String]) {
      println("testing small Map that doesn't promote to HashMap...")
      testMap(4, 2, 1000000)
      println()

      println("testing single-threaded HashMap use...")
      testMap(5, 1, 1000000)
      println()

      println("testing HashMap.size from multiple threads...")
      testMap(5, 2, 1000000)
      println()

      println("testing small Set that doesn't promote to HashSet...")
      testSet(4, 2, 1000000)
      println()

      println("testing single-threaded HashSet use...")
      testSet(5, 1, 1000000)
      println()

      println("testing HashSet.size from multiple threads...")
      testSet(5, 2, 1000000)
      println()
    }
}
