object Test {
  trait T[S]
  type Foo[S <: Foo[S]] = T[S]

  type X[A <: X[A]] = String
}
