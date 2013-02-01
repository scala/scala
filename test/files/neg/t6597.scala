object Test {
  trait T extends Any
  implicit case class Quux(value: Int) extends AnyVal with T
  object Quux
}
