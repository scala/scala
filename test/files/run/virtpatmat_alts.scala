object Test extends App {
  (true, true) match {
    case (true, true) | (false, false) => 1
  }

  List(5) match {
    case 1 :: Nil | 2 :: Nil  => println("FAILED")
    case (x@(4 | 5 | 6)) :: Nil => println("OK "+ x)
    case 7 :: Nil  => println("FAILED")
    case Nil  => println("FAILED")
  }
}