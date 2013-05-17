object Test {
  sealed trait A
  case object A1 extends A
}

trait Something[T]

case class Test() extends Something[Test.A]
