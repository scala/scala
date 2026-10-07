trait P {
  def p: E = E.X
}

object X extends P {
  override final val p = E.Y
}

object Test extends App {
  (X, X.p)
}
