// Partest does proper mixed compilation:
//   1. scalac *.scala *.java
//   2. javac *.java
//   3. scalc *.scala
//
// In the second scalc round, the classfile for A_1 is on the classpath.
// Therefore the inliner has access to the bytecode of `bar`, which means
// it can verify that the invocation to `bar` can be safely inlined.
//
// So both callsites of `flop` are inlined.
//
// In a single mixed compilation, `flop` cannot be inlined, see JUnit InlinerTest.scala, def mixedCompilationNoInline.

class B {
  @inline final def flop = A_1.bar
  def g = flop
}
class C {
  def h(b: B) = b.flop
}
