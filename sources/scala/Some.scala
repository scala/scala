package scala {
  final case class Some[c](x: c) extends Option[c] {
    def isNone = False;
    def get: c = x;
  }
}