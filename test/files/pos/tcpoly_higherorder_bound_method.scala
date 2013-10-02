trait SkolemisationOfHigherOrderBoundInMethod {
 def method[A, N[X <: A], M[X <: N[A]]]: Unit
}
