class t8372 {
  // failed with "error: tpe T1 is an unresolved spliceable type"; that was caused by
  // misguided type inference of type parameters in ArrayOps.unzip
  // the type inference failed because the order of implicit arguments was wrong
  // the evidence that T <: (T1, T2) came as last argument so it couldn't guide the
  // type inference early enough
  def unzip[T1, T2](a: Array[(T1, T2)]) = a.unzip
  // the same as above
  def unzip3[T1, T2, T3](a: Array[(T1, T2, T3)]): (Array[T1], Array[T2], Array[T3]) = a.unzip3
}
