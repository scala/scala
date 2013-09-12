package pack

object Test {
  import O.empty
  empty // this will trigger completion of `test`
        // with skolemizationLevel = 1
}

object O {
  // order matters (!!!)

  // this order breaks under 2.11.x
  def empty(implicit a: Any): Any = ???
  def empty[E]: C[E] = ???
}

abstract class C[E] {
  def foo[BB](f: BB)
  def test[B](f: B): Any = foo(f)
  // error: no type parameters for method foo: (<param> f: BB)scala.this.Unit exist so that it can be applied to arguments (B&1)
  // --- because ---
  // argument expression's type is not compatible with formal parameter type;
  // found   : B&1
  // required: ?BB
}
