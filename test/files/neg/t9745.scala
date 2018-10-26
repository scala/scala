class C {
  val func = Seq { x += 1; 42 } apply _
}

class D {
  var i = 0
  def f(n: Unit)(j: Int): Int = ???
  val g = x => f(y += 1)(x)
}

class E {
  var i = 0
  def f(n: Unit)(j: Int): Int = ???
  val g = x => f(x += 1)(x)
}

class Convo {
  def f(i: Int)(z: Any): Int = ???
  val g = (x, y) => f(42)(x, y)
}