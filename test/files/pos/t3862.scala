object OverloadingShapeType {
  // comment out this, and the other alternative is chosen.
  def blerg(f: String) {}

  def blerg[M[X], T](l: M[T]) {}

  blerg(List(1)) // error: type mismatch; found   : List[Int] required: String
}