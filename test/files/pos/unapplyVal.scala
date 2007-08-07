package test // bug #1215

class Async {
  def unapply(scrut: Any): Option[Any] = None
}

class Buffer {
  val Put = new Async
  //case class Put(x: int)

  def joinPat(x: Any): Unit = {
    x match {
      case Put =>
      case Put(y) =>
        println("returning "+y)
    }
  }
}
