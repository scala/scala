// scalac: -Wunused:imports,params
//
package p {
  object X
  class C {
    val x = 42
  }
  class D extends C {
    def y = 27
  }
  package q {
    object Y extends C {
      import Z.X      // unused due to ambiguity
      def f = X       // ambiguous if p.X defined in this file
      import Z.x      // unused because shadowed by inherited member defined in this file
      def g = x       // inherited member defined in this file
      def k = {
        import Z.x    // unused due to ambiguity
        x             // ambiguous if inherited member defined in this file
      }
    }
    object Test {
      def f(x: Int) = {
        new D { override def y = x }    // OK, inherited member defined in this file
      }
    }
    object Z extends C {
      object X
    }
  }
}
