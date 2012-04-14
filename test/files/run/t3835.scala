object Test {
  def main(args: Array[String]): Unit = {
    println((1, 2, 3) match { case (r, \u03b8, \u03c6) => r + \u03b8 + \u03c6 })
    val x = 1 match { case \u00e9 => \u00e9 } // store in local variable due to problem with inliner + virtpatmat
    println(x)
  }
}