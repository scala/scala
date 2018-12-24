object t4889 {

  import scala.language.higherKinds
  trait Matrix[S, +Repr[s] <: Matrix[s, Repr]] {
    def foo[R[S] >: Repr[S]](implicit ma1: MatrixAdder[S, R]) {}
  }

  trait SparseMatrix[S] extends Matrix[S, SparseMatrix]

  trait MatrixAdder[S, -R[_]] {
    def addTo(m: R[S]): Unit
  }

  implicit def adderImplicit[S, R[s] <: Matrix[s, R]] = new MatrixAdder[S, R] {
    def addTo(m: R[S]) = { }
  }

  val m1 = new SparseMatrix[Int] { }
  m1.foo

}
