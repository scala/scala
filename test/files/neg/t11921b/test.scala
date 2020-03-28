// scalac: -Wunused:imports,params
//
package p {
  package q {
    object Y extends C {
      import Z.X      // used
      def f = X       // OK
      import Z.x      // used
      def g = x       // OK
    }
    object Test {
      def f(x: Int) = {                   // unused
        new D { override def y = x }      // ambiguous
      }
    }
    object Z extends C {
      object X
    }
  }
}
