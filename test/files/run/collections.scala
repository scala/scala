import collection._
import scala.compat.Platform.currentTime

object Test extends Application {

  val printTime = false

  def sum[A](xs: Iterable[int]) = (0 /: xs)((x, y) => x + y)

  def time(op: => unit): unit = {
    val start = currentTime
    op
    if (printTime) Console.println("  time = "+(currentTime - start)+"ms")
  }

  def test(msg: String, s0: collection.immutable.Set[int], iters: int) = {
    Console.println("***** "+msg+":")
    var s = s0
    s = s + 2
    s = s + (3, 4000, 10000)
    Console.println("test1: "+sum(s))
    time {
      s = s ++ (List.range(0, iters) map (2*))
      Console.println("test2: "+sum(s)+", iters = "+iters)
    }
    time {
      var x = 0
      for (val i <- (0 to 10000))
        if (s contains i) x = x + i
      Console.println("test3: "+x)
    }
  }

  def test(msg: String, s0: collection.mutable.Set[int], iters: int) = {
    Console.println("***** "+msg+":")
    var s = s0
    s = s + 2
    s = s + (3, 4000, 10000)
    Console.println("test1: "+sum(s))
    time {
      s = s ++ (List.range(0, iters) map (2*))
      Console.println("test2: "+sum(s)+", iters = "+iters)
    }
    time {
      var x = 0
      for (val i <- (0 to 10000))
        if (s contains i) x = x + i
      Console.println("test3: "+x)
    }
  }

  def test(msg: String, s0: collection.immutable.Map[int, int], iters: int) = {
    Console.println("***** "+msg+":")
    var s = s0
    s = s + (2 -> 2)
    s = s + (3 -> 3, 4000 -> 4000, 10000 -> 10000)
    Console.println("test1: "+sum(s map (._2)))
    time {
      s = s ++ (List.range(0, iters) map (x => x * 2 -> x * 2))
      Console.println("test2: "+sum(s map (._2))+", iters = "+iters)
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
    if (iters == 5000) {
      time {
        var s1 = s
        var x = 0
        for (val i <- (0 to 10000)) {
          s get i match {
            case Some(i) => x = x + i
            case None =>
          }
          s1 = s1 + ((i + 10000) -> i)
        }
        Console.println("test4: "+x)
      }
    }
  }

  def test(msg: String, s0: collection.mutable.Map[int, int], iters: int) = {
    Console.println("***** "+msg+":")
    var s = s0
    s = s + (2 -> 2)
    s = s + (3 -> 3, 4000 -> 4000, 10000 -> 10000)
    Console.println("test1: "+sum(s map (._2)))
    time {
      s = s ++ (List.range(0, iters) map (x => x * 2 -> x * 2))
      Console.println("test2: "+sum(s map (._2))+", iters = "+iters)
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

  test("mutable.HashSet", new mutable.HashSet[int], 5000)
  test("immutable.Set", immutable.Set[int](), 5000)
  test("immutable.ListSet", new immutable.ListSet[int], 5000)
  test("immutable.TreeSet", new immutable.TreeSet[int], 5000)
  test("mutable.HashMap", new mutable.HashMap[int, int], 5000)
  test("immutable.Map", immutable.Map[int, int](), 5000)
  test("immutable.TreeMap", new immutable.TreeMap[int, int], 5000)
  test("immutable.ListMap", new immutable.ListMap[int, int], 3000)
  test("immutable.UnBalancedTreeMap", new immutable.UnbalancedTreeMap[int, int], 1000)
}
