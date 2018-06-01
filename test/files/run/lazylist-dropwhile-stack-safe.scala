object Test extends App {
  // Should not overflow the stack
  val lazyList = Iterator.iterate(LazyList.from(0))(_.dropWhile(_ => false)).drop(10000).next
  lazyList.head
  lazyList.tail.head

  val stream = Iterator.iterate(Stream.from(0))(_.dropWhile(_ => false)).drop(10000).next
  stream.head // superfluous, because `head` should be eager
  stream.tail.head
}
