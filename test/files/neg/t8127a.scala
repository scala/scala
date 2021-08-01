object H {
  def unapplySeq(m: Any): Seq[_] = List("")
}

object Test {
  def unapply(m: Any) = m match {
    case H(v) =>
    case _ =>
  }
}
  // later: OK
  // then: Seq[Any] is not a valid result type of an unapplySeq method of an extractor.
  // and:  The result type of an unapplySeq method must contain a member `get` to be used as an extractor pattern, no such member exists in Seq[Any]
  // now:  too many patterns for object H offering Boolean: expected 0, found 1
  // was:  result type Seq[_$2] of unapplySeq defined in method unapplySeq in object H does not conform to Option[_]
