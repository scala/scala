





object Test extends App {

  val lstv = List(1, 2, 3).view
  val lstvr = lstv.reverse
  assert(lstvr.iterator.toList == List(3, 2, 1))
  assert(lstvr.reverse == List(1, 2, 3))
  assert(lstvr.reverseIterator.toList == List(1, 2, 3))
  assert(lstvr.reverseMap(_ + 1) == List(2, 3, 4))

}









