trait Node { def eval: Int }

trait DebugNode extends Node {
  abstract override def eval: Int = {
    println("before")
    val res = super.eval
    println("res= "+res)
    res
  }
}

class Var extends Node { def eval = 42 }
class Foo { def eval = 42 }

class C {
  // typechecks, correct
  (new Var with DebugNode).eval

  // should *not* typecheck but does!
  // Foo.eval does not override Node.eval, but typechecker accepts this anyway
  (new Foo with DebugNode).eval
}
