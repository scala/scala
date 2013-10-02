class A {
  def f(xs: Array[Byte]) = xs.size
  def g(xs: Array[Byte]) = xs.length
}

class B {
  def f = "abc" map (_ + 1)
}

object C {
  final val x = "abc"

  implicit def convert(p: x.type): Int = 123

  math.max(122, x: Int)
}

class D {
  def f = (1 -> 2) + "c"
}

class Un {
  // forcing post-typer failure, since we're only interested in the output from the above
  def unimplemented: Int
}