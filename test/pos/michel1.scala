class A[Ta] (a : Ta) {
  def f = 1
}

trait C {}

class B[Tb] (b : Tb) extends C with A[Tb] (b) {
  def g = 2
}
