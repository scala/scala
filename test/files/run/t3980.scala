object A {
  def run1: Unit = {
    lazy val x: Unit = {(); println("once")}
    x
    x
  }
  def run2: Unit = {
	lazy val y: Int = 2
	println(y)
	println(y)
  }
}

object Test {
  def main(args: Array[String]) = {
	A.run1
	A.run2
  }
}
