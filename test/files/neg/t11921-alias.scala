object t1 {
  class C[T] { type TT = T }
  object O {
    type TT = String
    class D extends C[TT] {
      def n(x: TT) = x // OK
    }
  }
}

object t2 {
  class C[T] { type TT <: T }
  object O {
    type TT = String
    class D extends C[TT] {
      def n(x: TT) = x // ambiguous
    }
  }
}

object t3 {
  trait Context
  class A[C <: Context](val c: C)
  class B(val c: Context) { b =>
    val a = new A[c.type](c) {
      def n = c // OK
    }
  }
}

object t4 {
  trait Context
  class A[C <: Context](val c: C)
  class B(val c: Context) { b =>
    val a = new A(c) {
      def n = c // ambiguous
    }
  }
}
