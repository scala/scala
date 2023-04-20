class Test {
  trait F1[A, B] { def apply(a: A): B }
  trait S[T] { def map[R](f: F1[_ >: T, _ <: R]): S[R] }
  (??? : S[Int]).map(_.toString) // check that map's type param is inferred (should be String)
}
