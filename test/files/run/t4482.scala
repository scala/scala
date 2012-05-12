trait Foo { def i: Int }
trait Bar

case class Spam(i: Int) extends Foo with Bar

object Test {
  def matchParent(p:Any) = p match {
    case f:Foo if f.i == 1 => 1
    case _:Bar => 2
    case _:Foo => 3
  }
  def main(args: Array[String]): Unit = {
    println(matchParent(Spam(3)))
  }
}
