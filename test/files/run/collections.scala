import scala.collection._
import scala.compat.Platform.currentTime
import scala.language.postfixOps

object Test extends App {

  val printTime = false

  def sum[A](xs: Iterable[Int]) = (0 /: xs)((x, y) => x + y)

  def time(op: => Unit) {
    val start = currentTime
    op
    if (printTime) println("  time = "+(currentTime - start)+"ms")
  }

  def test(msg: String, s0: collection.immutable.Set[Int], iters: Int) = {
    println("***** "+msg+":")
    var s = s0
    s = s + 2
    s = s + (3, 4000, 10000)
    println("test1: "+sum(s))
    time {
      s = s ++ (List.range(0, iters) map (2*))
      println("test2: "+sum(s)+", iters = "+iters)
    }
    time {
      var x = 0
      for (i <- 0 to 10000)
        if (s contains i) x += i
      println("test3: "+x)
    }
  }

  def test(msg: String, s0: collection.mutable.Set[Int], iters: Int) = {
    println("***** "+msg+":")
    var s = s0
    s = s + 2
    s = s + (3, 4000, 10000)
    println("test1: "+sum(s))
    time {
      s = s ++ (List.range(0, iters) map (2*))
      println("test2: "+sum(s)+", iters = "+iters)
    }
    time {
      var x = 0
      for (i <- 0 to 10000)
        if (s contains i) x += i
      println("test3: "+x)
    }
  }

  def test(msg: String, s0: collection.immutable.Map[Int, Int], iters: Int) = {
    println("***** "+msg+":")
    var s = s0
    s = s + (2 -> 2)
    s = s + (3 -> 3, 4000 -> 4000, 10000 -> 10000)
    println("test1: "+sum(s map (_._2)))
    time {
      s = s ++ (List.range(0, iters) map (x => x * 2 -> x * 2))
      println("test2: "+sum(s map (_._2))+", iters = "+iters)
    }
    time {
      var x = 0
      for (i <- 0 to 10000)
        s get i match {
          case Some(i) => x += i
          case None =>
        }
      println("test3: "+x)
    }
    if (iters == 5000) {
      time {
        var s1 = s
        var x = 0
        for (i <- 0 to 10000) {
          s get i match {
            case Some(i) => x += i
            case None =>
          }
          s1 = s1 + ((i + 10000) -> i)
        }
        println("test4: "+x)
      }
    }
  }

  def test(msg: String, s0: collection.mutable.Map[Int, Int], iters: Int) = {
    println("***** "+msg+":")
    var s = s0
    s = s + (2 -> 2)
    s = s + (3 -> 3, 4000 -> 4000, 10000 -> 10000)
    println("test1: "+sum(s map (_._2)))
    time {
      s = s ++ (List.range(0, iters) map (x => x * 2 -> x * 2))
      println("test2: "+sum(s map (_._2))+", iters = "+iters)
    }
    time {
      var x = 0
      for (i <- 0 to 10000)
        s get i match {
          case Some(i) => x += i
          case None =>
        }
      println("test3: "+x)
    }
  }

  test("mutable.HashSet", new mutable.HashSet[Int], 5000)
  test("mutable.LinkedHashSet", new mutable.LinkedHashSet[Int], 5000)
  test("immutable.Set", immutable.Set[Int](), 5000)
  test("immutable.ListSet", new immutable.ListSet[Int], 5000)
  test("immutable.TreeSet", new immutable.TreeSet[Int], 5000)
  test("mutable.HashMap", new mutable.HashMap[Int, Int], 5000)
  test("mutable.LinkedHashMap", new mutable.LinkedHashMap[Int, Int], 5000)
  test("immutable.Map", immutable.Map[Int, Int](), 5000)
  test("immutable.TreeMap", new immutable.TreeMap[Int, Int], 5000)
  test("immutable.ListMap", new immutable.ListMap[Int, Int], 3000)
}
