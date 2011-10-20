object Test extends App {
  List(1,2,3) match {
    case Seq(x, y, z) => println(x * y * z)
  }
}