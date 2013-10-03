trait Value {}
case class FloatValue(x: Double) extends Value
object Test {
  def applyNumeric(op: (Double, Double) => Double):
    PartialFunction[List[Value], Value] = {
    case List(FloatValue(x), FloatValue(y)) => FloatValue(op(x, y))
  }
}
