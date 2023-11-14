package foo

object Usage{
  def ala(a: Sealed) = a match {
    case _: Base =>
      1
  }
}