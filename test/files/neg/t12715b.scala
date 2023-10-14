trait B {
 def f: Float = 1.0f
}

class A(override val f: Float) extends B

trait C extends B {
 abstract override val f = super.f + 100.0f
}

trait D extends B {
 abstract override val f = super.f + 1000.0f
}

object Test {
  def main(args: Array[String]): Unit = {
    new A(10.0f) with C with D {}
  }
}
