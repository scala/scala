object Test extends App {
  case class Number(i: Int)

  object UnliftNumber {
    def unapply(t: Any): Option[Number] = t match {
      case i: Int => Some(Number(i))
      case _ => None
    }
  }

  def eval(expr: Any): Option[Number] = expr match {
    case UnliftNumber(n) => Some(n)
    case _ => None
  }

  println(eval(1))
}
