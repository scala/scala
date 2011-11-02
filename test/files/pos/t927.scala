object Test {

  def sum(stream: Stream[Int]): Int =
    stream match {
      case Stream.Empty => 0
      case Stream.cons(hd, tl) => hd + sum(tl)
    }
  val str: Stream[Int] = List(1,2,3).iterator.toStream
  assert(sum(str) == 6)
  
}
