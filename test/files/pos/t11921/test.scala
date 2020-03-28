// scalac: -Werror

package p {
  package q {
    object Y extends C {
      import Z.X
      def f = X       // import beats foreign definition
      import Z.x
      def g = x       // import also beats foreign inheritance, was:
      //warning: imported `x` is permanently hidden by definition of value x in class C
    }
    object Z extends C {
      object X
    }
  }
}
