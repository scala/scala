// type parameter shadows actual type, massive overload error confuses.
class A(x: Int) { 
  def f[Int](y: Int) = x + y
}
