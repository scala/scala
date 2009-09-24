object Test {
  def implicitly[T](implicit t : T) = t
  implicit def perhaps[T](implicit t : T) : Option[T] = Some(t)
  implicit val hello = "Hello"
  implicitly[String]
  implicitly[Option[String]]
}
