trait T {
  type TT = T with Any
  private val priv = 0
  (??? : TT).priv
}

trait U {
  type UU = Any with U
  private val priv = 0
  (??? : UU).priv
}
