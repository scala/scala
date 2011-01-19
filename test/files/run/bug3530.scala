object Test {
  def main(args: Array[String]) {
    val list = List(1,2,3)
    list match {
      case List(_, _) => println("List with two elements")
      case List(_*) => println("Some List")
    }
  }
}