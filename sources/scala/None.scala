package scala {
  final case class None[b]() extends Option[b] {
    def isNone = True;
    def get: b = error("None does not have an element.");
  }
}