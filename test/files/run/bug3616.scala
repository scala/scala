object X extends Enumeration {
    val Y = Value
}
object Fruit extends Enumeration {
    val x = X.Y
    val A,B,C = Value
}
object Test {
  def main(args: Array[String]): Unit = {
    println(Fruit.values)
  }
}
