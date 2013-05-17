trait Matrix[@specialized A, Repr[C] <: Matrix[C, Repr]] { // crash goes away if @specialize is removed
  def duplicate(mb: MatrixBuilder[A, Repr]): Repr[A] = {
    mb.zeros
  }
}
trait DenseMatrix[@specialized A] extends Matrix[A, DenseMatrix]
trait DenseMatrixFlt extends DenseMatrix[Float]

trait MatrixBuilder[@specialized A, Repr[C] <: Matrix[C, Repr]] {
  def zeros: Repr[A]
}
object DenseFloatBuilder extends MatrixBuilder[Float, DenseMatrix] {
  val zeros = new Object with DenseMatrixFlt
  // Note:
  // - in 2.9 crash goes away if the explicit type "DenseMatrixFlt" is assigned to "zeros"
  // - in 2.9 crash goes away if DenseMatrixFlt is a class instead of a trait:
  //        val zeros = new DenseMatrixFlt
}

object Test extends App {
  val m1 = DenseFloatBuilder.zeros // in 2.9 crash goes away if explicit type "DenseMatrixFlt" is assigned to m1
  val m2 = m1.duplicate(DenseFloatBuilder)
}

