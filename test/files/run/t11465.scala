object Test extends App {
  def f = { println("F") ; 1 to 10 }
  println { f.foldLeft{ println("Z"); 0 }(_ + _) }
  println { (f.foldLeft{ println("Z"); 0 } _) (_ + _) }
}