//> using options -Ymacro-annotations
object Test extends App {

  @deprecated
  class Inner() {
  }

  lazy val Inner = new Inner()

  @deprecated
  class Inner2() {
  }

  val Inner2 = new Inner2()
}
