trait C[T]

trait A[T] {
  type EPC[X1] = C[X1]
  type EPC2[X1] = C[X1]
}

trait B[T] extends A[T] {
  override type EPC = C[T]
  override type EPC2[X1 <: String] = C[X1]
}