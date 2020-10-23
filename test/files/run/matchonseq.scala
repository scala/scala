object Test extends App {
  Vector(1,2,3) match {
    case head +: tail => println("It worked! head=" + head)
    case _            => println("Go back and use your noggin'")
  }
  Vector(1,2,3) match {
    case init :+ last => println("It worked! last=" + last)
    case _            => println("Ha ha.  You didn't last long..")
  }
}
