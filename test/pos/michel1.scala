class A[Ta] (a : Ta) {
  def f = 1
}

trait C {}

class B[Tb] (b : Tb) extends C with A[Tb] (b) with {
  def g = 2
}