trait F1[A, B] { def apply(a: A): B }

class Test {
  def foo[A](f1: F1[A, A]) = f1

  foo(x => x) // should result in only one error (the second one stemmed from adapting to SAM when the tree was erroneous)
}
