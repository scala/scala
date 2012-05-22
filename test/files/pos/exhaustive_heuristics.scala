// tests exhaustivity doesn't give warnings (due to its heuristic rewrites kicking in or it backing off)
object Test {
  // List() => Nil
  List(1) match {
    case List() =>
    case x :: xs =>
  }

  // we don't look into guards
  val turnOffChecks = true
  List(1) match {
    case _ if turnOffChecks =>
  }

  // TODO: we back off when there are any user-defined extractors
}