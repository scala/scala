



object Test {
  def main(args: Array[String]) {
    val gs = for (x <- (1 to 5)) yield { if (x % 2 == 0) List(1).seq else List(1).par }
    println(gs.flatten)
    println(gs.transpose)
    
    val s = Stream(Vector(1).par, Vector(2).par)
    println(s.flatten.toList)
    println(s.transpose.map(_.toList).toList)
  }
}
