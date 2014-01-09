object G {
  def unapply(m: Any): Option[_] = Some("")
}

object H {
  def unapplySeq(m: Any): Option[Seq[_]] = None
}

object Test {
  (0: Any) match {
    case G(v) => v
    case H(v) => v
    case _ =>
  }
}
