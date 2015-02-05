
class Free[A] {
  

  this match {
    case a @ Gosub() => gosub(a.a)(x => gosub(???)(???))
  }
  def gosub[A, B](a0: Free[A])(f0: A => Any): Free[B] = ???
}

  

  case class Gosub[B]() extends Free[B] {
    type C
    def a: Free[C] = ???
  }
