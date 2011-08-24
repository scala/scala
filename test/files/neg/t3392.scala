object Test {
  case class A(a: Int) {
    object Ex {
      def unapply(i: Int): Option[Int] = Some(i)
    }
  }

  A(42) match {
    case x@A(x/*<-- refers to the pattern that includes this comment*/.Ex(42)) =>
  }
}
