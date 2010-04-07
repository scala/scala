object Test {
  def main(args: Array[String]) = {
    val ary: Array[String] = Array("a", "b", "c")
    val lst: List[String] = List("a", "b", "c")
    val itr: Iterator[String] = lst.iterator
    val str: Stream[String] = lst.iterator.toStream

    Console.println(ary.zipWithIndex.toList)
    Console.println(lst.zipWithIndex.toList)
    Console.println(itr.zipWithIndex.toList)
    Console.println(str.zipWithIndex.toList)
    assert {
      ary.zipWithIndex match {
        case _: Array[Pair[_,_]] => true
        case _ => false
      }
    }

    val emptyArray = new Array[String](0)
    val emptyList: List[String] = Nil
    val emptyIterator = emptyList.iterator
    val emptyStream: Stream[String] = Stream.empty

    Console.println(emptyArray.zipWithIndex.toList)
    Console.println(emptyList.zipWithIndex.toList)
    Console.println(emptyIterator.zipWithIndex.toList)
    Console.println(emptyStream.zipWithIndex.toList)
  }
}
