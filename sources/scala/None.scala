package scala with {
  final case class None[b] extends Option[b] with {
    def isNone = True;
    def get: b = error("None does not have an element.");
  }
}