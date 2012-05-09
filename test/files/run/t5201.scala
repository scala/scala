object Test extends App {
  val seq = Seq(Seq(1, 2), Seq(3, 4)).view.flatten
  
  Console.println(seq.isInstanceOf[collection.SeqView[_,_]])
}
