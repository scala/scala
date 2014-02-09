class LM {
  class Node[B1]
  case class CC(n: LM)

  // crash
  val f: (LM => Any) = {
    case tttt =>
      new tttt.Node[Any]()
  }
}

object Test extends App {
  new LM().f(new LM())
}
