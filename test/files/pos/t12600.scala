// scalac: -Wunused:privates,locals,patvars

class Private {
  private type Curry[A] = { type T[B] = Either[A, B] }
  def m2[T[A]] :Unit = ()
  m2[Curry[Int]#T]
}