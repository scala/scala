// scalac: -Xfatal-warnings -unchecked
//
class Test {
  type ListInt = List[Int]
  List[Any]("") match {
    case is: ListInt => is.head
    case _ =>
  }
}
