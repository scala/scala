class LM {
  class Node[B1]

  // crash
  val g: (CC => Any) = {
    case CC(tttt) =>
      new tttt.Node[Any]()
  }

  val h: (Some[CC] => Any) = {
    case Some(CC(tttt)) =>
      new tttt.Node[Any]()
  }
}

object Test extends App {
  new LM().g(new CC(new LM()))
  new LM().h(Some(new CC(new LM())))
}
case class CC(n: LM)

