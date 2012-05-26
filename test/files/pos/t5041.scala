case class Token(text: String, startIndex: Int)

object Comment {
  def unapply(s: String): Option[Token] = None
}

object HiddenTokens {
  "foo" match { case Comment(_) => }
}
