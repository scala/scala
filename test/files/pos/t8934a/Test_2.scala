object Test {
  "" match {
    case Unapply(a, b) =>
      a: Int
      b: String
    case UnapplySeq(a, b1, b2) =>
      a: Int
      b1: String
      b2: String
  }
}
// These used to fail `too many patterns` under -Ymacro-expand:discard
