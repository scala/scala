object Test {
  def f[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](table: Tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T])(fun: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Unit) {
  }
  def g[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](table: Tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U])(fun: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Unit) {
  }

  def g20 = f(
    (  1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1)
  ) { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)) => () }

  def g21 = g(
      (1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,   1)
  ) { case ((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)) => () }
}
