
// as usual, the special import must be at package level

package p {
  package q {
    object X {
      import Predef.{identity=>_}
      def f() = 1 -> 2
      def g() = 42 ensuring (_ > 0)
    }
  }
}
