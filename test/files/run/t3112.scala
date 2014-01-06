// #3112
object Test {

  def main(args: Array[String]): Unit = {
    println((Vector() ++ (0 until 32)) take 0) // works
    println((Vector() ++ (0 until 33)) take 0) // error
    println((Vector() ++ (0 until 32)) takeRight 0) // works
    println((Vector() ++ (0 until 33)) takeRight 0) // error
  }

}