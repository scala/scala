


// Fixes #3422
object Test {

  def main(args: Array[String]) {
    val arr = Array.range(0,10)
    val map = arr groupBy (_%2)
    val v1 = map(0)
    val v2 = map(0)
    // this should hold, of course, assuming also that group by returns a regular
    // map implementation, and does nothing fancy - and it should return just a
    // hash map by default.
    assert(v1 eq v2)
  }

}
