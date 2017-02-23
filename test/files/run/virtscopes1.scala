object Test extends App {
  trait OptiML
  trait OptiMLExp { def apply(): Any; def result: Unit = { println(apply()) } }
  def OptiML[R](b: => R) = new Scope[OptiML, OptiMLExp, R](b)

  OptiML {
    object meh
    val f = (x: Int) => "foo "+ x
    f(10)
  }
}