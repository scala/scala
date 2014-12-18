class LM {
  class Node[B1]

  // crash
  val g: (CC => Any) = {
    case CC(tttt) =>
      tttt.## // no crash
      new tttt.Node[Any]()
  }
}

object Test extends App {
  new LM().g(new CC(new LM()))
}
case class CC(n: LM)

