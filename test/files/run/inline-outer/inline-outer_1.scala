// The inliner currently can't handle inline methods in separately
// compiled classes in the empty package.
package test

class C {
  var x: Any = "orig"

  final class D {
    @inline def skates(a: Any) = {
      x = a.toString
    }
  }
}
