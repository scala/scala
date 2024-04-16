//> using options -opt:inline:**
//
// Since 1.0.18, partest does mixed compilation only in two stages 
//   1. scalac *.scala *.java
//   2. javac *.java
//
// Before it used to do a third stage
//   3. scalc *.scala
//
// Because the inliner doesn't have access to the bytecode of `bar`, it cannot verify whether the
// invocation of `bar` can be safely copied to a different place, so `flop` is not inlined to `B.g`
// or `C.h`.

class B {
  @inline final def flop = A_1.bar
  def g = flop
}
class C {
  def h(b: B) = b.flop
}
