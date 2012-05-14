object Bug {
  def foo() {
    val v = {
      lazy val s = 0
      s
    }
  }
}
