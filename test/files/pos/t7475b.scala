trait U {
}

trait T {
  type TT = Any with T with U
  private val priv = 0
  (??? : TT).priv
}
