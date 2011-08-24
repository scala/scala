object Test {
  def main (args : Array[String]) {
    val b = new StringBuilder
    b.append ("Hello World!")
    b.lastIndexOf ('e')
    println (b.toString)
  }
}