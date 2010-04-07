object Test {
  abstract class Colour
  case object RED extends Colour
  case object YELLOW extends Colour
  val items = Array(RED, YELLOW)

  def main(args: Array[String]): Unit = items foreach println
}
