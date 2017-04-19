// this fails with naive attempts to fix scala/bug#8023
trait T[A <: T[A]]
