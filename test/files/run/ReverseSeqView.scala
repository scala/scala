object Test extends App {
  val lstv = List(1, 2, 3).view // SeqView
  val lstvr = lstv.reverse      // Can reverse a SeqView, but get a plain View which can no longer be reversed
  assert(lstvr.iterator.toList == List(3, 2, 1))
}
