object H {
  def unapplySeq(m: Any): Seq[_] = List("")
}

object Test {
  def unapply(m: Any) = m match {
    case H(v) =>
    case _ =>
  }
  // now:  too many patterns for object H offering Boolean: expected 0, found 1
  // was: result type Seq[_$2] of unapplySeq defined in method unapplySeq in object H does not conform to Option[_]
}
