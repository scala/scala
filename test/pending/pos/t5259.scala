object DefaultArgBogusTypeMismatch {

  class A[T]
  class B {
    type T = this.type
    def m(implicit a : A[T] = new A[T]) = a
  }
  
  def newB = new B
  val a1 = newB.m       // Bogus type mismatch

  val stableB = new B
  val a2 = stableB.m    // OK
}
