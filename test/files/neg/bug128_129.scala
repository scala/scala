object Test {
  def foo:int = {
    val x | 1 = 2; // not allowed
    x
  }

  class Bar {
    case class PT(x: PT) {
      def foo(x: Any) = x match {
        case PT(a@PT(a)) =>  // not allowed
      }
    }
  }

}
