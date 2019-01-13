trait A {
  def x: Double
  def y: Double

  def thisA: A
  def copy( x: Double = 0, y: Double = 0 ): A
}

class B( in: A ) {
  import in._

  def foo( a: Double, b: Double ) = a

  def bar = thisA.copy(
    x = foo(
      b = 1,
      a = 2 )
  )
}