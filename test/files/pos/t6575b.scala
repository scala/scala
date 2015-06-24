// inferred types were okay here as Function nodes aren't
// translated into anonymous subclasses of AbstractFunctionN
// until after the typer.
//
// So this test is just confirmation.
trait X { def foo: Function1[Int, Int] }

trait Y extends X {
  abstract override def foo = { case i => super.foo(i) * 2 }
}
trait Z extends X {
  abstract override def foo = { case i => super.foo(i) + 3 }
}

trait Comb extends Y with Z {
  abstract override def foo: Function1[Int, Int] = { case i => super.foo(i) - 2 }
}
