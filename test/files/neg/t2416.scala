object t2416a {
  trait A[X <: Double] { type B = X }
  def x : A[Int]#B = 10 // no you won't
}

object t2416b {
  trait A{type B[Y <: Double] = Int}
  def x : A#B[Boolean] = 10 // seriously?
}

object t2416c {
  trait A{type B[Z <: Double] = Int}
  type C[Z <: A] = Z#B[String] // nuh-uh!
}