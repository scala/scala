object Test {

  def sum(stream: Stream[int]): int =
    stream match {
      case Stream.empty => 0
      case Stream.cons(hd, tl) => hd + sum(tl)
    }
  val str: Stream[int] = Stream.fromIterator(List(1,2,3).elements)
  assert(sum(str) == 6)

}
