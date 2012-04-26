object D {
  def x(a: A) = if (a.isInstanceOf[B] || a.isInstanceOf[C]) ()
}

