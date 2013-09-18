trait M

case class N() extends M {
  def mytest(x: M) = x match {
    case A: N => 1
    case _    => 0
  }
}

