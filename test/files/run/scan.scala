object Test {

  def main(args: Array[String]): Unit = {
    val lst = List(1, 2, 3, 4, 5)

    assert(lst.scanLeft(0)(_ + _) == List(0, 1, 3, 6, 10, 15))
    assert(lst.scanRight(0)(_ + _) == List(15, 14, 12, 9, 5, 0))

    val emp = List[Int]()
    assert(emp.scanLeft(0)(_ + _) == List(0))
    assert(emp.scanRight(0)(_ + _) == List(0))

    val stream = LazyList(1, 2, 3, 4, 5)
    assert(stream.scanLeft(0)(_ + _) == LazyList(0, 1, 3, 6, 10, 15))

    assert(LazyList.from(1).scanLeft(0)(_ + _).take(5) == LazyList(0, 1, 3, 6, 10))
  }
}
