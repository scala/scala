object Test {
  def prettyPrintArray(x: Array[_]) = println("Array(" + x.mkString(", ") + ")")

  def main(args: Array[String]): Unit = {
    prettyPrintArray(Array(1,2,3) :+ 4)
    prettyPrintArray(1 +: Array(2,3,4))
    prettyPrintArray(Array() :+ 1)
    prettyPrintArray(1 +: Array())
  }
}

