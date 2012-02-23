class Outer {
  class Inner
}

object Test {
  val x = new Outer#Inner
}
