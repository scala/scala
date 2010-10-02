import collection._

object Test extends Application {

  val printTime = false

  def sum[A](xs: Iterable[Int]) = (0 /: xs)((x, y) => x + y)

  def time(op: => Unit): Unit = {
    val start = System.currentTimeMillis;
    op
    if (printTime) Console.println("  time = "+(System.currentTimeMillis - start)+"ms")
  }

  def test(msg: String, s0: collection.immutable.Set[Int]) = {
    Console.println("***** "+msg+":")
    var s = s0
    s = s + 2
    s = s + (3, 4000, 10000)
    Console.println("test1: "+sum(s))
    time {
      s = s ++ (List.range(0, 5000) map (2*))
      Console.println("test2: "+sum(s))
    }
    time {
      var x = 0
      for (val i <- (0 to 10000))
        if (s contains i) x = x + i
      Console.println("test3: "+x)
    }
  }

  def test(msg: String, s0: collection.mutable.Set[Int]) = {
    Console.println("***** "+msg+":")
    var s = s0
    s = s + 2
    s = s + (3, 4000, 10000)
    Console.println("test1: "+sum(s))
    time {
      s = s ++ (List.range(0, 5000) map (2*))
      Console.println("test2: "+sum(s))
    }
    time {
      var x = 0
      for (val i <- (0 to 10000))
        if (s contains i) x = x + i
      Console.println("test3: "+x)
    }
  }

  def test(msg: String, s0: collection.immutable.Map[Int, Int]) = {
    Console.println("***** "+msg+":")
    var s = s0
    s = s + (2 -> 2)
    s = s + (3 -> 3, 4000 -> 4000, 10000 -> 10000)
    Console.println("test1: "+sum(s map (._2)))
    time {
      s = s ++ (List.range(0, 1000) map (x => x * 2 -> x * 2))
      Console.println("test2: "+sum(s map (._2)))
    }
    time {
      var x = 0
      for (val i <- (0 to 10000))
        s get i match {
          case Some(i) => x = x + i
          case None =>
        }
      Console.println("test3: "+x)
    }
  }

  def test(msg: String, s0: collection.mutable.Map[Int, Int]) = {
    Console.println("***** "+msg+":")
    var s = s0
    s = s + (2 -> 2)
    s = s + (3 -> 3, 4000 -> 4000, 10000 -> 10000)
    Console.println("test1: "+sum(s map (._2)))
    time {
      s = s ++ (List.range(0, 5000) map (x => x * 2 -> x * 2))
      Console.println("test2: "+sum(s map (._2)))
    }
    time {
      var x = 0
      for (val i <- (0 to 10000))
        s get i match {
          case Some(i) => x = x + i
          case None =>
        }
      Console.println("test3: "+x)
    }
  }

  test("immutable.ListSet", new immutable.ListSet[Int])
  test("immutable.TreeSet", new immutable.TreeSet[Int])
  test("mutable.HashSet", new mutable.HashSet[Int])
  test("immutable.ListMap", new immutable.ListMap[Int, Int])
  test("immutable.TreeMap", new immutable.TreeMap[Int, Int])
  test("immutable.UnBalancedTreeMap", new immutable.UnbalancedTreeMap[Int, Int])
  test("immutable.HashTreeSet", new immutable.HashTreeSet[Int])
  test("immutable.HashTreeMap", new immutable.HashTreeMap[Int, Int])
  test("mutable.HashMap", new mutable.HashMap[Int, Int])
}
