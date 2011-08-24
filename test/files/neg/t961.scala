object Temp {
  abstract class A
  object B {
    private case class B_inner() extends A
    def apply: A = B_inner()
    def unapply(a: A) = a match {
      case B_inner() => true
      case _ => false
    }
  }
  B() match {
    case B() => Console.println("match")
  }
}
