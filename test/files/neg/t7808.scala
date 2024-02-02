
class C {
  type OI = Option[Int]
  def f(z: OI, ls: List[OI], rs: List[OI]): (List[OI], List[OI]) = {
    val (ls, rs) = z match {
      case Some(_) => (z::ls, rs)
      case _       => (ls, z::rs)
    }
    (ls, rs)
  }
}

/*
t7808.scala:5: error: recursive value x$1 needs type
    val (ls, rs) = z match {
         ^
1 error
*/
