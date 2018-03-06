class Show[T](val i: Int)
object Show {
  def apply[T](implicit st: Show[T]): Int = st.i

  implicit val showInt: Show[Int] = new Show[Int](0)
  // Now that the fix for scala/bug#10545 has been merged the fallback
  // instance can live at this level and is beaten by Show[Int] due to
  // specificity
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
