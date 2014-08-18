object Main {

  case class IntAndDouble(i: Int, d: Double)

  // a.productElement used to be Int => Double
  // now: Int => AnyVal
  val a = IntAndDouble(1, 5.0)
  val d: Double = a.productElement(0)
}
