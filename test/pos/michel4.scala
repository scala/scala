class A() {
  val f : Int = 2
}

class B() extends A() with {
  override val f : Int = super.f
}