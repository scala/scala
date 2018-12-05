object Test extends App {

  val unsortedArray = Array[Int](1,-1,-100,0,100,99,91,10,91)
  val sortedArray = unsortedArray.sortWith(_ < _)

  assert(sortedArray.mkString(" ") == "-100 -1 0 1 10 91 91 99 100")
  assert(unsortedArray.mkString(" ") == "1 -1 -100 0 100 99 91 10 91")

}
