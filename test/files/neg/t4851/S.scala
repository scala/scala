object Test {
  val x1 = new J
  val x2 = new J()
  val x3 = new J(1, 2, 3, 4, 5)

  val y1 = Some(1, 2, 3)
  val y2 = new Some(1, 2, 3)
  
  val z1 = new J2
  val z2 = new J2()
  val z3 = new J2(())

  def main(args: Array[String]): Unit = {
    println(x1)
    println(x2)
    println(x3)
    println(y1)
    
    println(z1)
    println(z2)
    println(z3)
  }
}
