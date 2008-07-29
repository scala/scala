object A {
  def f :Int = {
    class B {
      println("B")
      return 10
    }
    new B
    20
  }
  def main(args: Array[String]) {
    f
  }
}
