object test {
  type A = List[A]
  type B[T] = List[B[B[T]]]
  trait I { type E }
  type C = I { type E = C }
}
