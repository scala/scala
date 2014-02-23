
trait Test {
  type +[A, B] = (A, B)
  type *[A, B] = (A, B)

  type X[A, B] = A + B
  type Y[A, B] = A * B
  type Z[A, B] = A `*` B
}
