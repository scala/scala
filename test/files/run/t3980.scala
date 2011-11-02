object A {
  def run1 {
    lazy val x: Unit = {(); println("once")}
    x
    x 
  }
  def run2 {
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