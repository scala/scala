class Show[T](val i: Int)
object Show extends Show0 {
  def apply[T](implicit st: Show[T]): Int = st.i

  implicit val showInt: Show[Int] = new Show[Int](0)
}

trait Show0 {
  // if the fix for scala/bug#10545 was merged this could be moved up
  // to object Show and would be beaten by Show[Int] due to specificity
  implicit def fallback[T]: Show[T] = new Show[T](1)
}

class Generic
object Generic {
  implicit val gen: Generic = new Generic
  implicit def showGen[T](implicit gen: Generic): Show[T] = new Show[T](2)
}

object Test extends App {
  assert(Show[Int] == 0)
  assert(Show[String] == 1)
  assert(Show[Generic] == 2) // showGen beats fallback due to longer argument list
}
