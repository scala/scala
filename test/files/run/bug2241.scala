object Test extends Application {
  def f(a:Array[Int]) = a match {
    case Array(1, _*) => "yes"
    case _ => "no"
  }
  assert(f(null) == "no")
}