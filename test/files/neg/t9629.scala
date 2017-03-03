class Test {
  def foo(a: Any) {
    a match {
      case Option[Int] => // error was issued before
      case Some(Option[Int]) => // error was skipped, patmat issued an internal error

      // variations
      case (_, Option[Int]) =>
      case x @ (y @ Option[Int]) =>
    }
  }
}
