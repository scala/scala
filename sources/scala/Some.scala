package scala {
  final case class Some[c](x: c) extends Option[c] {
    def isNone = false;
    def get: c = x;
  }
}
