



object Test {
  
  class Inv[T]
  
  def foo[S](interface: Inv[_ >: S], implementation: Inv[S]) {}

  def bar[R, T <: R](interface: Inv[R], impl: Inv[T]) { 
    //foo[T](interface, impl)
    foo(interface, impl) // Compilation Error
    // Inv[R] <: Inv[_ >: S]
    // Inv[T] <: Inv[S]
    // ----------------------
    // R >: S
    // T == S
  }

}


