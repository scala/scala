object o {
  val props = new scala.collection.mutable.ListBuffer[(String,String)]
  lazy val property = new {
    def update(propName: String, p: String) = props += ((propName, p))
  }
  def crash = property("a") = "b"
}