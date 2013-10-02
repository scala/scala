object Test extends App {
  val y = (0: Int) match {
    case 1 => 1
    case 0 | 0 => 0
    case 2 | 2 | 2 | 3 | 2 | 3 => 0
    case _ => -1
  }
  assert(y == 0, y)
}
