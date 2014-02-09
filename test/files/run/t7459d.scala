class LM {
  class Node[B1]
  case class CC(n: LM)

  // crash
  val f: (LM => Any) = {
    case tttt =>
      val uuuu: (tttt.type, Any) = (tttt, 0)
      new uuuu._1.Node[Any]()
  }
}

object Test extends App {
  new LM().f(new LM())
}
