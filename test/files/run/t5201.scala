object Test extends App {
  // Now make sure we really get a view
  val seq = Seq(Seq(1, 2), Seq(3, 4)).view.flatten
  Console.println(seq.isInstanceOf[collection.View[_]])
}
