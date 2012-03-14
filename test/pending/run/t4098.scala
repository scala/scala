class A(a: Any) { 
  def this() = { this(b) ; def b = new {} }
}

object Test {
  def main(args: Array[String]): Unit = {
    new A ("")
  }
}
