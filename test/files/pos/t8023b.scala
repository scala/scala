// this fails with naive attempts to fix SI-8023
trait T[A <: T[A]]
