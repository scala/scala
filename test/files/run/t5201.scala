object Test extends App {
  // First make sure specific types are preserved
  val tmp: Vector[Int] = Vector(Vector(1,2), Vector(3,4)).view.flatten.force

  // Now make sure we really get a view
  val seq = Seq(Seq(1, 2), Seq(3, 4)).view.flatten
  Console.println(seq.isInstanceOf[collection.SeqView[_,_]])
}
