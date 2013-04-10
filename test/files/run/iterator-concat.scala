object Test {
  // Create `size` Function0s, each of which evaluates to an Iterator
  // which produces 1. Then fold them over ++ to get a single iterator,
  // which should sum to "size".
  def mk(size: Int): Iterator[Int] = {
    val closures = (1 to size).toList.map(x => (() => Iterator(1)))
    closures.foldLeft(Iterator.empty: Iterator[Int])((res, f) => res ++ f())
  }
  def main(args: Array[String]): Unit = {
    println(mk(100).sum)
    println(mk(1000).sum)
    println(mk(10000).sum)
    println(mk(100000).sum)
  }
}
