object Bug {
  def foo(): Unit = {
    val v = {
      lazy val s = 0
      s
    }
  }
}
