class T {
  def send(o: Any, d: Int = 10): Unit = { }

  def c(f: => Any): Unit = { }

  def f(): Unit = {
    var a = this
    a.send(
      c(a.send(()))
    )
  }
}
