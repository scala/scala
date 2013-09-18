


object Test {

  def time[U](b: =>U): Long = {
    val start = System.currentTimeMillis
    b
    val end = System.currentTimeMillis

    end - start
  }

  def main(args: Array[String]) {
    val sz = 1000000000

    val range = 1 to sz
    check { assert(range.min == 1, range.min) }
    check { assert(range.max == sz, range.max) }

    val descending = sz to 1 by -1
    check { assert(descending.min == 1) }
    check { assert(descending.max == sz) }

    val numeric = 1.0 to sz.toDouble by 1
    check { assert(numeric.min == 1.0) }
    check { assert(numeric.max == sz.toDouble) }

    val numdesc = sz.toDouble to 1.0 by -1
    check { assert(numdesc.min == 1.0) }
    check { assert(numdesc.max == sz.toDouble) }
  }

  def check[U](b: =>U) {
    val exectime = time {
      b
    }

    // whatever it is, it should be less than, say, 250ms
    // if `max` involves traversal, it takes over 5 seconds on a 3.2GHz i7 CPU
    //println(exectime)
    assert(exectime < 250, exectime)
  }

}
