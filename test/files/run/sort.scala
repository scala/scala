object Test extends App {
  println((1 to 100000).toList.sortWith(_<_).length)
  println(List(1, 5, 10, 3, 2).toList.sortWith(_<_))
  println(List(1, 5, 10, 3, 2).toList.sortWith(_>_))
  println(List(10).toList.sortWith(_<_))
  println(List(10,9).toList.sortWith(_<_))
  println(List[Int]().toList.sortWith(_<_))
}

