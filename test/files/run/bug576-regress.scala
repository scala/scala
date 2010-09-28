class A {
  override def equals(other: Any) = other match {
    case _: this.type => true
    case _            => false
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val x1 = new A
    val x2 = new A
    assert(x1 == x1)
    assert(x1 != x2)
    assert(x1 != ())
    assert(x2 != x1)
  }
}