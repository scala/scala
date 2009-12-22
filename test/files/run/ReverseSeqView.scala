





object Test extends Application {

  val lstv = List(1, 2, 3).view
  val lstvr = lstv.reverse
  lstvr.iterator
  lstvr.reverse
  lstvr.reverseIterator
  lstvr.reverseMap(_ + 1)

}









