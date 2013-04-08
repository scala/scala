object Test extends App {
  val x: PartialFunction[Int, Int] = { case 1 => 1 }
  val o: Any = ""
  assert(x.applyOrElse(0, (_: Int) => o) == "")
}
