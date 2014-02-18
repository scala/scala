import language._

object Test extends App {
  def b = new AnyRef {
    def a= ()
  }
  b.a match { case _ => () }
}
