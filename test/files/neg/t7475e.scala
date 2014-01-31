trait U {
}

trait Base {
  private val priv = 0

  type TT = U with T // should exclude `priv`
  (??? : TT).priv
}

trait T extends Base {
}
