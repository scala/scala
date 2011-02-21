object Test extends App {
  println((1 to 100000).toList.sort(_<_).length)
  println(List(1, 5, 10, 3, 2).toList.sort(_<_))
  println(List(1, 5, 10, 3, 2).toList.sort(_>_))
  println(List(10).toList.sort(_<_))
  println(List(10,9).toList.sort(_<_))
  println(List[Int]().toList.sort(_<_))
}

