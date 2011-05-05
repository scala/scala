class T {
  def send(o: Any, d: Int = 10) { }

  def c(f: => Any) { }

  def f() {
    var a = this
    a.send(
      c(a.send(()))
    )
  }
}
