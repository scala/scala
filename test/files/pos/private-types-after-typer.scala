// Testing that the type of the outer accessor in O2
// doesn't crash the compiler over private type escaping scope.
trait T {
  class C {
     private object O1 {
        object O2
     }
  }
}