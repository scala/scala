// cf t6880
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

// was
// java.lang.NoSuchFieldError: c
//         at C.<init>(t12002.scala:6)
