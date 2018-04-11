/*
 * Test case for scala/bug#4835.  This tests confirm that the fix
 * doesn't break laziness.  To test memory consumption,
 * I need to confirm that OutOfMemoryError doesn't occur.
 * I could create such tests.  However, such tests consume
 * too much time and memory.
 */
object Test {
  private final val INFINITE = -1
  def testLazyListIterator(num: Int, stream: LazyList[Int]): Unit = {
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
    import LazyList.{from, cons, empty}
    testLazyListIterator(INFINITE, from(0))
    testLazyListIterator(INFINITE, from(0).filter(_ % 2 == 1))
    testLazyListIterator(1, LazyList(1))
    testLazyListIterator(2, LazyList(1, 2))
    //LazyList with side effect
    testLazyListIterator(2, cons(1, cons({ print(" A"); 2}, empty)))
    testLazyListIterator(3, LazyList(1, 2, 3))
    //LazyList with side effect
    testLazyListIterator(3, cons(1, cons({ print(" A"); 2}, cons({ print(" B"); 3}, LazyList.empty))))
  }
}
