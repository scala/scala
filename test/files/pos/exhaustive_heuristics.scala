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

  // we back off when there are any user-defined extractors
  // in fact this is exhaustive, but we pretend we don't know since List's unapplySeq is not special to the compiler
  // to compensate our ignorance, we back off
  // well, in truth, we do rewrite List() to Nil, but otherwise we do nothing
  // the full rewrite List(a, b) to a :: b :: Nil, for example is planned (but not sure it's a good idea)
  List(true, false) match {
    case List(_, _, _*) =>
    case List(node, _*)  =>
    case Nil =>
  }

}