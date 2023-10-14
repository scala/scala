trait A {
  def f: String
}

trait B extends A {
  def f = "B";
}

trait C extends A {
  override val f = "C"
}

trait D extends C {
  override val f = "D"
}

trait E extends A with B {
  def d = super.f
}

object O1 extends B with C with D with E
object O2 extends B with C with E with D
object O3 extends B with E with C with D
