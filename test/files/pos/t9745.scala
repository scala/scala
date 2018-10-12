class C {
  val func = Seq { var i = 0; i += 1; i } apply _
}

class D {
  var i = 0
  def f(n: Unit)(j: Int): Int = ???
  val g = x => f(i += 1)(x)
}

class Convo {
  def f(i: Int)(z: Any): Int = ???
  val g = (x: Int, y: Int) => f(42)(x, y)
}