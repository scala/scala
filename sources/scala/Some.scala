package scala {
  final case class Some[c](x: c) extends Option[c] with {
    def isNone = False;
    def get: c = x;
  }
}