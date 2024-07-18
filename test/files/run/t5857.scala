//> using options -deprecation
//
import scala.math.Ordering.Double.IeeeOrdering


object Test {

  def time[U](b: => U): Long = {
    val start = System.currentTimeMillis
    b
    val end = System.currentTimeMillis

    end - start
  }

  def main(args: Array[String]): Unit = {
    val sz = 1000000000

    val range = 1 to sz
    check { assert(range.min == 1, range.min) }
    check { assert(range.max == sz, range.max) }

    val descending = sz to 1 by -1
    check { assert(descending.min == 1) }
    check { assert(descending.max == sz) }
  }

  def check[U](b: => U): Unit = {
    val exectime = time {
      b
    }

    // whatever it is, it should be less than, say, 250ms
    // if `max` involves traversal, it takes over 5 seconds on a 3.2GHz i7 CPU
    //println(exectime)
    assert(exectime < 250, exectime)
  }

}
