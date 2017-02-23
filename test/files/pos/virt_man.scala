trait App {
  type Foo[T]
  abstract class Record
  type DenseVector[T]
  implicit def mV[T:Manifest]: Manifest[DenseVector[T]]
  val z: Foo[DenseVector[Double]] = error("")
  type R = Record{val x: DenseVector[Double]}
  val m = manifest[R]
}
