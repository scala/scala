/*
 * Test case for SI-4835.  This tests confirm that the fix
 * doesn't break laziness.  To test memory consumption,
 * I need to confirm that OutOfMemoryError doesn't occur.
 * I could create such tests.  However, such tests consume
 * too much time and memory.
 */
object Test {
  private final val INFINITE = -1
  def testStreamIterator(num: Int, stream: Stream[Int]): Unit = {
    val iter = stream.iterator
    print(num)
    // if num == -1, then steram is infinite sequence
    if (num == INFINITE) {
      for(i <- 0 until 10) {
        print(" " + iter.next())
      }
    } else {
      while(iter.hasNext) {
        print(" " + iter.next())
      }
    }
    println()
  }

  def main(args: Array[String]): Unit = {
    import Stream.{from, cons, empty}
    testStreamIterator(INFINITE, from(0))
    testStreamIterator(INFINITE, from(0).filter(_ % 2 == 1))
    testStreamIterator(1, Stream(1))
    testStreamIterator(2, Stream(1, 2))
    //Stream with side effect
    testStreamIterator(2, cons(1, cons({ print(" A"); 2}, empty)))
    testStreamIterator(3, Stream(1, 2, 3))
    //Stream with side effect
    testStreamIterator(3, cons(1, cons({ print(" A"); 2}, cons({ print(" B"); 3}, Stream.empty))))
  }
}
