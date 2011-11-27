package test

object Main {
  def main(args: Array[String]) {
    val xs = List(1, 2, 3, 4)
    xs match { //(xs: @unchecked) match {
      case x::xs => println(x)
    }
    Predef.exit(0) //deprecated in 2.9.0
  }
}
