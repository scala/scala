class A {
  // fail
  val a =
    for {
      case Some(x) <- List(Some(1), None)
    } yield x

  // fail
  val b =
    for {
      Some(x) <- List(Some(1), None)
      case y = x + 1
    } yield x+y
}
