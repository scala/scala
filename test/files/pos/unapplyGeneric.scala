object Bar {
  def unapply[A,B](bar:Bar[A,B]) = Some(bar)
}

class Bar[A,B](val _1:A, val _2:B) extends Product2[A,B] {
  def canEqual(other: Any) = other.isInstanceOf[Bar[_,_]]
}

object Test {
  new Bar(2, 'a') match {
    case Bar(x,y) =>
  }
}
