object C {
  def main(args: Array[String]): Unit = {
    val duck = B.foo
    println("duck: " + duck) // Need to use duck in an expression to see if it crashes or not
  }
}
