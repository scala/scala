trait T {
  def test: Unit = {
    byName("".toString)
    ()
  }

  @inline
  final def byName(action: => Unit) = action
}

object Test extends App {
  (new T {}).test
}
