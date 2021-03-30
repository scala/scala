// scalac: -Xsource:3
//
class A {
  // ok
  val a =
    for {
      case Some(x) <- List(Some(1), None)
      y = x + 1
    } yield x + y

  // ok
  val b =
    for {
      Some(x) <- List(Some(1), None)
      Some(y) <- List(None, Some(2))
    } yield x+y

  // fail
  val c =
    for {
      case Some(x) <- List(Some(1), None)
      case y = x + 1
    } yield x + y
}
