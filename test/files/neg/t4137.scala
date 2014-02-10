trait C[T]

trait A[T] {
  type EPC[X1]            = C[X1]
  type EPC2[X1 <: String] = C[X1]
  type EPC3[X1 <: AnyRef] = C[X1]

  trait Trait1[X1] extends C[X1]
  trait Trait2[X1 <: String] extends C[X1]
  trait Trait3[X1 <: AnyRef] extends C[X1]
}

trait B[T] extends A[T] {
  override type EPC = C[T]
  override type EPC2[X1 <: AnyRef] = C[X1]
  override type EPC3[X1 <: String] = C[X1]

  trait Trait1[X1] extends super.Trait1[X1]
  trait Trait2[X1 <: AnyRef] extends super.Trait2[X1]
  trait Trait3[X1 <: String] extends super.Trait3[X1]
}
