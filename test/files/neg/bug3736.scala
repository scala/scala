object Test {
  class A {
    def f1 = super.toString
    def f2 = super.isInstanceOf[String]
    def f3 = super.asInstanceOf[AnyRef]
    def f4 = super.==(new AnyRef)
    def f5 = super.!=(new AnyRef)
    def f6 = super.##
  }

  def main(args: Array[String]): Unit = {
    val x = new A
    x.f1
    x.f2
    x.f3
    x.f4
    x.f5
    x.f6
  }
}

