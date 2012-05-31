// check the interaction between constants and type tests in creating the equality axioms
object Test {
  type Formula = List[String]
  val TrueF: Formula = List()
  def distribute(a: Formula, b: Formula) = (a, b) match {
    case (TrueF, _) =>
    case (_, TrueF) =>  // bug: considered unreachable
    case (a :: Nil, b :: Nil) =>
    case _ =>
  }
}