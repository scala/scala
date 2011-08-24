object Test {
  type T = PartialFunction[String,String]
  def g(h: T) = ()
  g({case s: String => s})
}
