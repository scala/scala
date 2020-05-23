class C(private[this] var c: String) {
  private[this] var x: String = _
  c = "good"
  x = c + " boy!"
  override def toString = x
}

object Test {
  def main(args: Array[String]) = println {
    new C("bad")
  }
}
