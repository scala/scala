trait T[A, B] {
  def m[R]: T[R, B] = null
}

// gets a mixin forwarder `def m[R]: T[R<method-tparam>, R<class-tparam>]
// so the type parameter in the mixin forwarder is renamed
class C[X, R] extends T[X, R]
