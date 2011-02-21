
object NegativeId extends Enumeration {
  val Negative = Value(-1, "minus")
  val Zero = Value(0, "zero")
  val Positive = Value(1, "plus")

  def fromInt(id: Int) = values find (_.id == id) match {
    case Some(v) => v
    case None => null
  }
}

object Test extends App {
  println(NegativeId.fromInt(-1))
  println(NegativeId.fromInt(0))
  println(NegativeId.fromInt(1))
}
