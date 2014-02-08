trait U {
  private val priv = 0
  type TT = U with T // should allow `priv`
  (??? : TT).priv
}

trait Base {

}

trait T extends Base {

}
