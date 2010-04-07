class Term
case class Num(n: Int) extends Term
case class Add(x: Term, y: Term) extends Term

object Value {
  def unapply(term: Any): Boolean = true
}

object Test {
  def main(args: Array[String]) {
    val term = Add(Num(1), Add(Num(2), Num(3)))
    val res = term match {
      case Add(Num(x), Num(y))    => "Add(Num, Num)"
      case Add(Value(), y)        => "Add(Value, ?)"
      case _                      => "?"
    }
    assert(res == "Add(Value, ?)")
  }
}
