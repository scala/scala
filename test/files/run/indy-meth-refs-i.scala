// scalac: -Ydelambdafy:method-ref
class C {
  def foo = 0
}

class D extends C {
  override def foo = 1
  def bar = () => super.foo
}

object Test {
  def main(args: Array[String]): Unit = {
    val d = new D
    val obtained = d.bar.apply()
    assert(obtained == 0, obtained)
  }
}
