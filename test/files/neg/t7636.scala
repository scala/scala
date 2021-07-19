object Main extends App {
  class Foo[A](x: A)
  object bar extends Foo(5: T forSome { type T })
}
// Inference of the existential type for the parent type argument A still fails.
// That looks tricky to fix, see the comments in scala/bug#7636.
// But we at least prevent a cascading NPE.
