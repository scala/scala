class A[Ta] (a : Ta) {
  def f = 1
}

trait C {}

class B[Tb] (b : Tb) extends A[Tb] (b) with C {
  def g = 2
}
