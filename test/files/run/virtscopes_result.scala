object Test extends App {
  trait OptiML
  trait OptiMLExp[X] {
    def apply: X
    def result: X = {
      val r = apply
      println(r)
      r
    }
  }
  def OptiML[R](b: => R) = new Scope[OptiML, OptiMLExp[R], R](b)

  val x: String = OptiML {
    object meh
    val f = (x: Int) => "foo "+ x
    f(10)
  }
  println("x out of block: " + x)
}
