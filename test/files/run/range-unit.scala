import scala.collection.immutable.Range

object Test {
  // ha ha, I always forget math.abs(Int.MinValue) == Int.MinValue
  val numbers = (
    ( (-3 to 3) ++ List(17, 127, Int.MaxValue, Int.MinValue + 1)
    ).distinct.sortBy(n => (math.abs(n), n))
  ) :+ Int.MinValue

  // reducing output a little
  val endpoints = numbers filterNot Set(-3, -2, 2, 17, 127)

  def num(n: Int) = {
    val frommax = Int.MaxValue - n
    val frommin = Int.MinValue - n

    if (n > 0) {
      if (frommax == 0) "MAX"
      else if (frommax < 1000) "MAX-" + frommax
      else "" + n
    }
    else {
      if (frommin == 0) "MIN"
      else if (frommin > -1000) "MIN+" + (-frommin)
      else "" + n
    }
  }

  def run[T](body: => Range): List[Any] = {
    try   { val r = body ; if (r.isEmpty) List(r.length) else List(num(r.length), num(r.head), num(r.last)) }
    catch { case e: IllegalArgumentException => List("---\n    " + e) }
  }

  def runGroup(label: String, f: (Int, Int, Int) => Range) {
    println(">>> " + label + " <<<\n")
    for (start <- endpoints) {
      val s = "%-7s %-7s %-7s %s".format("start", "end", "step", "length/first/last")
      println(s  + "\n" + ("-" * s.length))
      for (end <- endpoints ; step <- numbers) {
        print("%-7s %-7s %-7s ".format(num(start), num(end), num(step)))
        println(run(f(start, end, step)).mkString("/"))
      }
      println("")
    }
  }

  def main(args: Array[String]): Unit = {
    runGroup("Range.inclusive", Range.inclusive(_, _, _))
    runGroup("Range.apply", Range.apply(_, _, _))
    runGroup("start to end", (x, y, _) => x to y)
    runGroup("start to end by step", _ to _ by _)
    runGroup("start until end", (x, y, _) => x until y)
    runGroup("start until end by step", _ until _ by _)
  }
}
