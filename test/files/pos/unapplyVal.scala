package test // bug #1215

class Async {
  def unapply(scrut: Any): Option[Any] = None
}

class Buffer {
  val Put = new Async
  //case class Put(x: Int)

  def joinPat(x: Any): Unit = {
    x match {
      case Put => 
      case Put(y) =>
        println("returning "+y)
    }
  }
}


object unapplyJoins extends App { // bug #1257

  class Sync {
    def apply(): Int = 42
    def unapply(scrut: Any): Boolean = false
  }

  class Buffer {
    object Get extends Sync

    val jp: PartialFunction[Any, Any] = {
      case Get() => 
    }
  }

  println((new Buffer).jp.isDefinedAt(42))
}
