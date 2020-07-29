// cf t6880
class C(private[this] var c: String) {
  private[this] var x: String = _
  c = "good"
  x = c + " boy!"
  override def toString = x
}

class D(private[this] var a: Int) {
  println(a)
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C("bad"))
    new D(27)
  }
}

// was
// java.lang.NoSuchFieldError: c
//         at C.<init>(t12002.scala:6)
