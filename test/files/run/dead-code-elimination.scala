
// This testcase is a snippet that did not compile correctly under
// pre-release 2.10.x. The relevant discussion around it can be
// found at:
// https://groups.google.com/forum/?fromgroups#!topic/scala-internals/qcyTjk8euUI[1-25]
//
// The reason it did not compile is related to the fact that ICode
// ops did not correctly define the stack entries they consumed and
// the dead code elimination phase was unable to correctly reconstruct
// the stack after code elimination.
//
// Originally, this did not compile, but I included it in the run
// tests because this was ASM-dependent and did not happen for GenJVM.
//
// Thus, we run the code and force the loading of class B -- if the
// bytecode is incorrect, it will fail the test.

final class A {
  def f1 = true
  def f2 = true
  @inline def f3 = f1 || f2
  class B {
    def f() = 1 to 10 foreach (_ => f3)
  }
  def f = (new B).f()
}

object Test {
  def main(args: Array[String]): Unit = {
    // force the loading of B
    (new A).f
  }
}
