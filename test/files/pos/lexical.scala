// #2081
class RichInt(n: Int) {
  def days = 1000*60*60*24*n
}

object Test extends Application {
  implicit def RichInt(n: Int): RichInt = new RichInt(n)
  println(10.days)
}
