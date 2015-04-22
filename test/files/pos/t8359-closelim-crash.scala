package test

// This is a minimization of code that crashed the compiler during bootstrapping
// in the first iteration of https://github.com/scala/scala/pull/4373, the PR
// that adjusted the order of free and declared params in LambdaLift.

// Was:
//  java.lang.AssertionError: assertion failed:
//  Record Record(<$anon: Function1>,Map(value a$1 -> Deref(LocalVar(value b)))) does not contain a field value b$1
// at scala.tools.nsc.Global.assert(Global.scala:262)
// at scala.tools.nsc.backend.icode.analysis.CopyPropagation$copyLattice$State.getFieldNonRecordValue(CopyPropagation.scala:113)
// at scala.tools.nsc.backend.icode.analysis.CopyPropagation$copyLattice$State.getFieldNonRecordValue(CopyPropagation.scala:122)
// at scala.tools.nsc.backend.opt.ClosureElimination$ClosureElim$$anonfun$analyzeMethod$1$$anonfun$apply$2.replaceFieldAccess$1(ClosureElimination.scala:124)
class Typer {
  def bar(a: Boolean, b: Boolean): Unit = {
    @inline
    def baz(): Unit = {
      ((_: Any) => (Typer.this, a, b)).apply("")
    }
    ((_: Any) => baz()).apply("")
  }
}

