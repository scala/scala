// bug #2187
object Test extends Application {
  def foo(xs:List[String]) = xs match {
    case Seq(x) => x
    case Seq(x,y) => ""
  }
}