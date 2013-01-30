class Y[T](val i: Option[T]) extends AnyVal {
   def q: List[T] = {
      lazy val e: List[T] = i.toList
      e
   }
}
