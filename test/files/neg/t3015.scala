class UnApp[P] {
  def unapply(a: P): Option[P] = Some(a)
}

object Test extends Application {
  val b: UnApp[_] = new UnApp[String]
  val b(foo) = "foo" 
}
