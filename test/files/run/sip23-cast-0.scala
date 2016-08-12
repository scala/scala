object Test extends App {
  def check[T](body: => T, ok: Boolean): Unit =
    try {
      body
      assert(ok)
    } catch {
      case cce: ClassCastException =>
        assert(!ok)
    }

  trait Global
  final val global0 = new Global {}
  final val global1 = new Global {}

  check(global0.asInstanceOf[global0.type], true)
  check(global0.asInstanceOf[global1.type], true)
}
