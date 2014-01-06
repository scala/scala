object Test extends App {
  Vector(1,2,3) match {
    case head +: tail => println("It worked! head=" + head)
  }
  Vector(1,2,3) match {
    case init :+ last => println("It worked! last=" + last)
  }
}
