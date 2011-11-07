object D {
  def x(a: A) = 
    a match {
      case _: B => ()
      case _: C => ()
    }
}

