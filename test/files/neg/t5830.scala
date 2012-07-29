import scala.annotation.switch

class Test {
  def unreachable(ch: Char) = (ch: @switch) match {
    case 'a' => println("b") // ok
    case 'a' => println("b") // unreachable
    case 'c' =>
  }
}